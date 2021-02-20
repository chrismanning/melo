{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Transaction where

import Control.Concurrent.Classy
import Control.Exception.Safe as E
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool as P
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import Melo.Common.Logging

class Monad m => Transaction m where
  withTransaction :: (Pg.Connection -> m a) -> m a

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTransControl t,
    Transaction m
  ) =>
  Transaction (t m)
  where
  withTransaction ma = do
    result <- liftWith \run ->
      withTransaction $ run <$> ma
    restoreT (pure result)

newtype TransactionIOT m a = TransactionIOT {runTransactionIOT :: ReaderT (Pool Pg.Connection) m a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadBase b,
      MonadBaseControl b,
      MonadIO,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl
    )

instance
  ( MonadIO m,
    MonadMask m
  ) =>
  Transaction (TransactionIOT m)
  where
  withTransaction t =
    TransactionIOT $
      ReaderT $ \pool ->
        E.mask $ \restore -> do
          (conn, localPool) <- liftIO $ takeResource pool
          liftIO $ Pg.begin conn
          r <- restore (runTransaction pool $ t conn) `E.onException` (liftIO $ cleanUp conn localPool)
          liftIO $ Pg.commit conn
          liftIO $ putResource localPool conn
          return r
    where
      cleanUp :: Pg.Connection -> LocalPool Pg.Connection -> IO ()
      cleanUp conn localPool = do
        $(logWarnShowIO) ("rolling back" :: String)
        liftIO $ do
          Pg.rollback conn `E.catch` \(_ :: IOError) -> return ()
          putResource localPool conn

runTransaction :: Pool Pg.Connection -> TransactionIOT m a -> m a
runTransaction conn = (flip runReaderT) conn . runTransactionIOT

class Monad m => Savepoint m where
  withSavepoint :: m a -> m a

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTransControl t,
    Savepoint m
  ) =>
  Savepoint (t m)
  where
  withSavepoint ma = do
    result <- liftWith \run ->
      withSavepoint $ run ma
    restoreT (pure result)

newtype SavepointIOT m a = SavepointIOT {runSavepointIOT :: ReaderT Pg.Connection m a}
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl
    )

instance
  ( MonadIO m,
    Logging m,
    MonadMask m
  ) =>
  Savepoint (SavepointIOT m)
  where
  withSavepoint t = SavepointIOT $
    ReaderT $ \conn -> do
      E.mask $ \restore -> do
        sp <- liftIO $ Pg.newSavepoint conn
        r <- restore (runSavepoint conn $ t) `E.onException` cleanUp conn sp
        liftIO (Pg.releaseSavepoint conn sp) `E.catch` \err -> do
          $(logError) $ "release savepoint failed: " <> show err
          if Pg.isFailedTransactionError err
            then liftIO $ Pg.rollbackToAndReleaseSavepoint conn sp
            else E.throwIO err
        return r
    where
      cleanUp conn sp = do
        $(logWarnShow) ("rolling back savepoint" :: String)
        liftIO $ Pg.rollbackToAndReleaseSavepoint conn sp

runSavepoint :: Pg.Connection -> SavepointIOT m a -> m a
runSavepoint conn = flip runReaderT conn . runSavepointIOT
