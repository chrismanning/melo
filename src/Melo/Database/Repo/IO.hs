{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Database.Repo.IO where

import Melo.Common.Exception
import Control.Foldl (PrimMonad)
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Kind
import Data.Maybe
import Data.Pool
import Data.Vector (Vector, empty, fromList)
import Hasql.Connection
import Hasql.CursorTransactionIO
import Hasql.Session
import Hasql.Streaming
import Melo.Common.Logging
import Melo.Database.Repo
import qualified Rel8
import Rel8 ((==.))
import Streaming.Prelude (Stream, Of)
import Witch

data DbConnection = Single Connection | Pooled (Pool Connection)

data RepositoryHandle a = RepositoryHandle
  { connSrc :: DbConnection,
    tbl :: Rel8.TableSchema (a Rel8.Name),
    pk :: forall f. Entity (a Rel8.Result) => a f -> Rel8.Column f (PrimaryKey (a Rel8.Result)),
    upsert :: Maybe (Rel8.Upsert (a Rel8.Name))
  }

newtype RepositoryIOT (t :: (Type -> Type) -> Type) m a = RepositoryIOT
  { runRepositoryIOT :: ReaderT (RepositoryHandle t) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader (RepositoryHandle t),
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTransControl,
      PrimMonad
    )

instance MonadTrans (RepositoryIOT a) where
  lift = RepositoryIOT . lift

instance
  ( MonadIO m,
    Entity (a Rel8.Result),
    From (NewEntity (a Rel8.Result)) (a Rel8.Expr),
    Rel8.Rel8able a,
    Rel8.Serializable (a Rel8.Expr) (a Rel8.Result),
    Rel8.Table Rel8.Expr (a Rel8.Expr),
    Rel8.Table Rel8.Name (a Rel8.Name),
    Rel8.DBEq (PrimaryKey (a Rel8.Result))
  ) =>
  Repository (a Rel8.Result) (RepositoryIOT a m)
  where
  getAll = do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc $ Rel8.each tbl
  getByKey ks = do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      let keys = Rel8.lit <$> ks
      all <- Rel8.each tbl
      Rel8.where_ $ pk all `Rel8.in_` keys
      pure all
  insert es | null es = pure empty
  insert es = do
    RepositoryHandle {connSrc, tbl, upsert} <- ask
    fromList <$> runInsert
      connSrc
      Rel8.Insert
        { into = tbl,
          rows = Rel8.values (from <$> es),
          onConflict = fromMaybe Rel8.Abort (Rel8.DoUpdate <$> upsert),
          returning = Rel8.Projection (\x -> x)
        }
  insert' es | null es = pure 0
  insert' es = do
    RepositoryHandle {connSrc, tbl, upsert} <- ask
    runInsert
      connSrc
      Rel8.Insert
        { into = tbl,
          rows = Rel8.values (from <$> es),
          onConflict = fromMaybe Rel8.Abort (Rel8.DoUpdate <$> upsert),
          returning = fromIntegral <$> Rel8.NumberOfRowsAffected
        }
  delete ks | null ks = pure empty
  delete ks = do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    let keys = Rel8.lit <$> ks
    let d = Rel8.Delete {
      from = tbl,
      using = pure (),
      deleteWhere = \_ row -> pk row `Rel8.in_` keys,
      returning = Rel8.Projection pk
    }
    $(logDebugIO) $ Rel8.showDelete d
    let session = statement () $ Rel8.delete d
    fromList <$> case connSrc of
      Single conn' -> liftIO $ run session conn' >>= either throwIO pure
      Pooled pool -> liftIO $ withResource pool $ \conn -> run session conn >>= either throwIO pure
  update es | null es = pure empty
  update es = do
    us <- forM es $ \e ->
      doUpdate (from e) (Rel8.Projection (\x -> x))
    pure $ fromList (concat us)
  update' es | null es = pure ()
  update' es = forM_ es $ \e -> doUpdate (from e) (pure ())

doUpdate :: (
  MonadReader (RepositoryHandle t') m,
  Rel8.Rel8able t',
  MonadIO m,
  Rel8.Serializable (t' Rel8.Expr) (t' Rel8.Result),
  Entity (t' Rel8.Result),
  Rel8.DBEq (PrimaryKey (t' Rel8.Result))
  ) => t' Rel8.Result -> Rel8.Returning (t' Rel8.Name) b -> m b
doUpdate e ret = do
  RepositoryHandle {connSrc, tbl, pk} <- ask
  let u = Rel8.Update {
    target = tbl,
    from = pure (),
    set = \_ _ -> Rel8.lit e,
    updateWhere = \_ row -> pk row ==. Rel8.litExpr (pk e),
    returning = ret
  }
  $(logDebugIO) $ Rel8.showUpdate u
  let session = statement () $ Rel8.update u
  case connSrc of
    Single conn' -> liftIO do
      run session conn' >>= \case
        Left e -> do
          -- TODO add `Rel8.showUpdate u` as context with katip
          $(logErrorIO) $ displayException e
          throwIO e
        Right r -> pure r
    Pooled pool -> liftIO do
      withResource pool $ \conn ->
        run session conn >>= \case
          Left e -> do
            -- TODO add `Rel8.showUpdate u` as context with katip
            $(logErrorIO) $ displayException e
            throwIO e
          Right r -> pure r

runSelect ::
  (MonadIO m, Rel8.Serializable exprs (Rel8.FromExprs exprs)) =>
  DbConnection ->
  Rel8.Query exprs ->
  m (Vector (Rel8.FromExprs exprs))
runSelect connSrc q = do
--  $(logDebugIO) $ Rel8.showQuery q
  let session = statement () $ Rel8.selectVector q
  case connSrc of
    Single conn' -> liftIO $ run session conn' >>= either throwIO pure
    Pooled pool -> liftIO $ withResource pool $ \conn -> run session conn >>= either throwIO pure

selectStream :: (
  Rel8.Serializable exprs (Rel8.FromExprs exprs)
  ) => Rel8.Query exprs -> Stream (Of (Rel8.FromExprs exprs)) (CursorTransactionIO s) ()
selectStream q = streamingQuery (Rel8.select q) ()

runInsert :: MonadIO m => DbConnection -> Rel8.Insert a -> m a
runInsert connSrc i = do
--  $(logDebugIO) $ Rel8.showInsert i
  let session = statement () $ Rel8.insert i
  case connSrc of
    Single conn' -> liftIO $ run session conn' >>= either throwIO pure
    Pooled pool -> liftIO $ withResource pool $ \conn -> run session conn >>= either throwIO pure

runInsert' :: MonadIO m => DbConnection -> Rel8.Insert a -> m (Either QueryError a)
runInsert' connSrc i = do
--  $(logDebugIO) $ Rel8.showInsert i
  let session = statement () $ Rel8.insert i
  case connSrc of
    Single conn' -> liftIO $ run session conn'
    Pooled pool -> liftIO $ withResource pool $ \conn -> run session conn

infixl 4 `startsWith`
startsWith :: Rel8.Expr s -> Rel8.Expr s -> Rel8.Expr Bool
startsWith = Rel8.function "starts_with"
