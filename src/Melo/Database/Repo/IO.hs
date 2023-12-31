{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Database.Repo.IO where

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Monad.Trans.Cont
import Data.ByteString.Char8 qualified as C8
import Data.Pool
import Data.Text qualified as T
import Data.Time.Clock
import Data.Typeable
import Data.Vector qualified as V
import Data.Word
import Hasql.Connection as Hasql
import Hasql.CursorTransactionIO
import Hasql.CursorTransactionIO.TransactionIO
import Hasql.Session
import Hasql.Statement as Hasql
import Hasql.Streaming
import Hasql.TransactionIO.Sessions
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Database.Repo
import Melo.Env qualified as Env
import OpenTelemetry.Trace qualified as Otel
import Rel8 ((==.))
import Rel8 qualified
import Streaming.Prelude qualified as S

data RepositoryHandle a = RepositoryHandle
  { tbl :: Rel8.TableSchema (a Rel8.Name),
    pk :: forall f. Entity (a Rel8.Result) => a f -> Rel8.Column f (PrimaryKey (a Rel8.Result)),
    upsert :: Maybe (Rel8.Upsert (a Rel8.Name))
  }
  deriving Typeable

data Config = Config
  { host :: C8.ByteString,
    port :: Word16,
    user :: C8.ByteString,
    password :: C8.ByteString,
    database :: C8.ByteString,
    pool :: ConnectionPoolConfig
  }
  deriving (Show, Typeable)

data ConnectionPoolConfig = ConnectionPoolConfig
  { maxIdleTime :: NominalDiffTime,
    maxConnections :: Int
  }
  deriving (Show, Typeable)

instance From Env.Database Config where
  from envDb = Config {
    host = envDb.host.unwrap,
    port = envDb.port.unwrap,
    user = envDb.user.unwrap,
    password = envDb.password.unwrap,
    database = envDb.database.unwrap,
    pool = from envDb.pool
  }

instance From Env.DatabaseConnectionPool ConnectionPoolConfig where
  from envPool = ConnectionPoolConfig {
    maxIdleTime = envPool.maxIdleTime.unwrap,
    maxConnections = envPool.maxConnections.unwrap
  }

newtype ConnectionPool = ConnectionPool (Pool Connection)
  deriving (Typeable)

getConnectionPool ::
  ( AppDataReader m,
    MonadIO m,
    MonadThrow m
  ) =>
  m (Pool Connection)
getConnectionPool =
  getAppData @ConnectionPool >>= \case
    Just (ConnectionPool pool) -> pure pool
    Nothing ->
      getAppData @Config >>= \case
        Nothing -> throwM DatabaseNotConfigured
        Just config -> do
          let connInfo = Hasql.settings config.host config.port config.user config.password config.database
          let newConnection =
                Hasql.acquire connInfo >>= \case
                  Left e -> throwIO (ConnectionError e)
                  Right conn -> pure conn
          pool <- liftIO $ newPool $ defaultPoolConfig newConnection Hasql.release (realToFrac config.pool.maxIdleTime) config.pool.maxConnections
          putAppData (ConnectionPool pool)
          pure pool

getRepoHandle ::
  forall e m.
  ( AppDataReader m,
    MonadThrow m,
    Typeable e
  ) =>
  m (RepositoryHandle e)
getRepoHandle = getAppData @(RepositoryHandle e) >>= throwOnNothing (EntityNotConfigured (typeRep (Proxy @(e Rel8.Result))))

instance
  {-# OVERLAPPABLE #-}
  ( Entity (a Rel8.Result),
    Typeable a,
    From (NewEntity (a Rel8.Result)) (a Rel8.Expr),
    Rel8.Rel8able a,
    Rel8.Serializable (a Rel8.Expr) (a Rel8.Result),
    Rel8.Table Rel8.Expr (a Rel8.Expr),
    Rel8.Table Rel8.Name (a Rel8.Name),
    Rel8.DBEq (PrimaryKey (a Rel8.Result))
  ) =>
  Repository (a Rel8.Result) (AppM IO IO)
  where
  getAll = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @a
    withSpan ("getAll$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool $ Rel8.each tbl
  getByKey ks | length ks == 1 = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @a
    withSpan ("getByKey$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        all <- Rel8.each tbl
        let k = Rel8.lit $ V.head ks
        Rel8.where_ $ pk all ==. k
        pure all
  getByKey ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @a
    withSpan ("getByKey$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        let keys = Rel8.lit <$> ks
        all <- Rel8.each tbl
        Rel8.where_ $ pk all `Rel8.in_` keys
        pure all
  insert es | null es = pure mempty
  insert es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, upsert} <- getRepoHandle @a
    withSpan ("insert$" <> T.pack tbl.name.name) defaultSpanArguments do
      runInsert
        pool
        Rel8.runVector
        Rel8.Insert
          { into = tbl,
            rows = Rel8.values (from <$> es),
            onConflict = fromMaybe Rel8.Abort (Rel8.DoUpdate <$> upsert),
            returning = Rel8.Returning (\x -> x)
          }
  insert' es | null es = pure 0
  insert' es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, upsert} <- getRepoHandle @a
    withSpan ("insertN$" <> T.pack tbl.name.name) defaultSpanArguments do
      runInsert
        pool
        Rel8.runN
        Rel8.Insert
          { into = tbl,
            rows = Rel8.values (from <$> es),
            onConflict = fromMaybe Rel8.Abort (Rel8.DoUpdate <$> upsert),
            returning = Rel8.NoReturning
          }
  delete ks | null ks = pure mempty
  delete ks | length ks == 1 = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @a
    withSpan' ("delete$" <> T.pack tbl.name.name) defaultSpanArguments \span -> do
      let d =
            Rel8.Delete
              { from = tbl,
                using = pure (),
                deleteWhere = \_ row -> pk row ==. Rel8.lit (ks V.! 0),
                returning = Rel8.Returning pk
              }
      do
        let statement = Rel8.showDelete d
        Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
        $(logDebugVIO ['statement]) "Executing DELETE"
      let session = statement () $ Rel8.runVector $ Rel8.delete d
      liftIO do
        withResource pool $ \conn -> run session conn >>= throwOnLeft
  delete ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @a
    withSpan' ("delete$" <> T.pack tbl.name.name) defaultSpanArguments \span -> do
      let keys = Rel8.lit <$> ks
      let d =
            Rel8.Delete
              { from = tbl,
                using = pure (),
                deleteWhere = \_ row -> pk row `Rel8.in_` keys,
                returning = Rel8.Returning pk
              }
      do
        let statement = Rel8.showDelete d
        Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
        $(logDebugVIO ['statement]) "Executing DELETE"
      let session = statement () $ Rel8.runVector $ Rel8.delete d
      liftIO do
        withResource pool $ \conn -> run session conn >>= throwOnLeft
  update es | null es = pure mempty
  update es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @a
    withSpan ("update$" <> T.pack tbl.name.name) defaultSpanArguments do
      us <- forM es $ \e ->
        doUpdate h pool Rel8.runVector (from e) (Rel8.Returning (\x -> x))
      pure (msum us)
  update' es | null es = pure ()
  update' es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @a
    withSpan ("update'$" <> T.pack tbl.name.name) defaultSpanArguments do
      forM_ es $ \e -> doUpdate h pool Rel8.run_ (from e) (Rel8.NoReturning)

doUpdate ::
  forall t' m b a.
  ( Rel8.Rel8able t',
    MonadIO m,
    Tracing m,
    Rel8.Serializable (t' Rel8.Expr) (t' Rel8.Result),
    Entity (t' Rel8.Result),
    Rel8.DBEq (PrimaryKey (t' Rel8.Result))
  ) =>
  RepositoryHandle t' ->
  Pool Connection ->
  (Rel8.Statement b -> Hasql.Statement () a) ->
  t' Rel8.Result ->
  Rel8.Returning (t' Rel8.Name) b ->
  m a
doUpdate (RepositoryHandle {tbl, pk}) pool runner e ret = do
  withSpan' ("doUpdate$" <> T.pack tbl.name.name) defaultSpanArguments \span -> do
    let u =
          Rel8.Update
            { target = tbl,
              from = pure (),
              set = \_ _ -> Rel8.lit e,
              updateWhere = \_ row -> pk row ==. Rel8.litExpr (pk e),
              returning = ret
            }
    do
      let statement = Rel8.showUpdate u
      Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
      $(logDebugVIO ['statement]) "Executing UPDATE"
    let session = statement () $ runner $ Rel8.update u
    liftIO do
      withResource pool $ \conn ->
        run session conn >>= \case
          Left e -> do
            let statement = Rel8.showUpdate u
            let cause = displayException e
            $(logErrorVIO ['statement, 'cause]) "UPDATE failed"
            throwIO e
          Right r -> pure r

runSelect ::
  (MonadIO m, Tracing m, Rel8.Serializable exprs (Rel8.FromExprs exprs)) =>
  Pool Connection ->
  Rel8.Query exprs ->
  m (Vector (Rel8.FromExprs exprs))
runSelect pool q = withSpan' "runSelect" defaultSpanArguments \span -> do
  do
    let statement = Rel8.showQuery q
    Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
    $(logDebugVIO ['statement]) "Executing SELECT"
  let session = statement () $ Rel8.runVector $ Rel8.select q
  liftIO $ withResource pool $ \conn -> run session conn >>= throwOnLeft

runSelectM ::
  ( AppDataReader m,
    MonadIO m,
    MonadThrow m,
    Tracing m,
    Rel8.Serializable exprs (Rel8.FromExprs exprs)
  ) =>
  Rel8.Query exprs ->
  m (Vector (Rel8.FromExprs exprs))
runSelectM q = do
  pool <- getConnectionPool
  runSelect pool q

data StreamItem a =
    RowItem a
  | EndOfStream
  | StreamError SomeException

selectStream ::
  forall exprs m.
  ( Rel8.Serializable exprs (Rel8.FromExprs exprs),
    AppDataReader m,
    MonadConc m,
    MonadIO m
  ) =>
  Rel8.Query exprs ->
  ContT () m (S.Stream (S.Of (Rel8.FromExprs exprs)) m ())
selectStream query = do
  pool <- getConnectionPool
  (conn, localPool) <- liftIO $ takeResource pool
  (inChan, outChan) <- liftIO $ newChan 1
  readThreadId <- lift $ fork do
    finally
      do
        catch
          do
            streamSession conn inChan >>= either throwM pure
          \e'@(SomeException e) -> do
            let cause = displayException e
            $(logErrorVIO ['cause]) $ "Error while streaming results"
            liftIO $ writeChan inChan (StreamError e')
      do
        liftIO do
          writeChan inChan EndOfStream
    pure ()
  ContT \f -> f () `finally` killThread readThreadId
  ContT \f -> f () `finally` liftIO (putResource localPool conn)
  ContT \f -> f () `finally` ($(logInfoIO) $ "Finished streaming results")
  pure (S.reread (liftIO . (handleItem <=< readChan)) outChan)
  where
    handleItem :: StreamItem (Rel8.FromExprs exprs) -> IO (Maybe (Rel8.FromExprs exprs))
    handleItem (RowItem a) = pure $ Just a
    handleItem EndOfStream = pure $ Nothing
    handleItem (StreamError (SomeException e)) = throwIO e
    streamSession conn inChan = do
      liftIO $ run (transactionIO ReadCommitted ReadOnly NotDeferrable (cursorTransactionIO (processStream inChan (streamQuery query)))) conn
    processStream inChan s = do
      $(logInfoIO) $ "Starting streaming results"
      (s & S.map RowItem >> S.yield EndOfStream)
          & S.mapM_ (\s -> liftIO (writeChan inChan s))
    streamQuery :: Rel8.Query exprs -> S.Stream (S.Of (Rel8.FromExprs exprs)) (CursorTransactionIO s) ()
    streamQuery q = do
      let statement = showt (Rel8.showQuery q)
      $(logDebugVIO ['statement]) "Streaming SELECT"
      streamingQuery (Rel8.run $ Rel8.select q) ()

runInsert :: (MonadIO m, Tracing m) => Pool Connection -> (Rel8.Statement b -> Hasql.Statement () a) -> Rel8.Insert b -> m a
runInsert pool runner i = withSpan' "runInsert" defaultSpanArguments \span -> do
  do
    let statement = Rel8.showInsert i
    Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
    $(logDebugVIO ['statement]) "Executing INSERT"
  let session = statement () $ runner $ Rel8.insert i
  liftIO $ withResource pool $ \conn -> run session conn >>= throwOnLeft

runInsert' :: (MonadIO m, Tracing m) => Pool Connection -> (Rel8.Statement b -> Hasql.Statement () a) -> Rel8.Insert b -> m (Either QueryError a)
runInsert' pool runner i = withSpan' "runInsert'" defaultSpanArguments \span -> do
  do
    let statement = Rel8.showInsert i
    Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
    $(logDebugVIO ['statement]) "Executing INSERT"
  let session = statement () $ runner $ Rel8.insert i
  liftIO $ withResource pool $ \conn -> run session conn

infixl 4 `startsWith`

startsWith :: Rel8.Expr Text -> Rel8.Expr Text -> Rel8.Expr Bool
startsWith a b = Rel8.function "starts_with" (a, b)
