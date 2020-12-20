{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Transaction where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Exception
import qualified Control.Exception.Safe as E
import Data.Pool as P
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.Transaction as Pg
import Melo.Common.Effect
import Melo.Common.Logging

data Transaction :: Effect where
  WithTransaction :: (Pg.Connection -> m a) -> Transaction m a

withTransaction :: Has Transaction sig m => (Pg.Connection -> m a) -> m a
withTransaction t = send (WithTransaction t)

newtype TransactionC m a = TransactionC {runTransactionC :: m a}
  deriving newtype (Applicative, Functor, Monad)

instance
  (Has (Lift IO) sig m, Has Logging sig m, Has (Reader (Pool Pg.Connection)) sig m) =>
  Algebra (Transaction :+: sig) (TransactionC m)
  where
  alg hdl (L (WithTransaction t)) ctx = do
    pool <- ask
    mask $ \restore -> do
      (conn, localPool) <- sendIO $ takeResource pool
      sendIO $ Pg.begin conn
      r <- restore (hdl (t conn <$ ctx)) `onException` cleanUp conn localPool
      sendIO $ Pg.commit conn
      sendIO $ putResource localPool conn
      return r
    where
      cleanUp conn localPool = do
        $(logWarnShow) ("rolling back" :: String)
        sendIO $ do
          Pg.rollback conn `E.catch` \(_ :: IOError) -> return ()
          putResource localPool conn
  alg hdl (R other) ctx = TransactionC (alg (runTransactionC . hdl) other ctx)

runTransaction :: TransactionC m a -> m a
runTransaction = runTransactionC

data Savepoint :: Effect where
  WithSavepoint :: m a -> Savepoint m a

withSavepoint :: (Has Savepoint sig m) => m a -> m a
withSavepoint t = send (WithSavepoint t)

newtype SavepointC m a = SavepointC {runSavepointC :: m a}
  deriving newtype (Applicative, Functor, Monad)

instance
  (Has (Lift IO) sig m, Has Logging sig m, Has (Reader Pg.Connection) sig m) =>
  Algebra (Savepoint :+: sig) (SavepointC m)
  where
  alg hdl (L (WithSavepoint t)) ctx = do
    conn <- ask
    mask $ \restore -> do
      sp <- sendIO $ Pg.newSavepoint conn
      r <- restore (hdl (t <$ ctx)) `onException` cleanUp conn sp
      sendIO (Pg.releaseSavepoint conn sp) `catch` \err -> do
        $(logError) $ "release savepoint failed: " <> show err
        if Pg.isFailedTransactionError err
          then sendIO $ Pg.rollbackToAndReleaseSavepoint conn sp
          else throwIO err
      return r
    where
      cleanUp conn sp = do
        $(logWarnShow) ("rolling back savepoint" :: String)
        sendIO $ Pg.rollbackToAndReleaseSavepoint conn sp
  alg hdl (R other) ctx = SavepointC (alg (runSavepointC . hdl) other ctx)

runSavepoint :: SavepointC m a -> m a
runSavepoint = runSavepointC
