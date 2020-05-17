{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Transaction where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import qualified Control.Exception.Safe as E
import Data.Pool as P
import qualified Database.PostgreSQL.Simple as Pg
import Melo.Common.Effect
import Melo.Common.Exception

data Transaction :: Effect where
  WithTransaction :: ReaderC Pg.Connection m a -> Transaction m a

withTransaction :: (Has Transaction sig m) => ReaderC Pg.Connection m a -> m a
withTransaction t = send (WithTransaction t)

newtype TransactionC m a = TransactionC {runTransactionC :: ReaderC (Pool Pg.Connection) m a}
  deriving newtype (Applicative, Functor, Monad)

instance
  (Has (Lift IO) sig m) =>
  Algebra (Transaction :+: sig) (TransactionC m)
  where
  alg hdl (L (WithTransaction t)) ctx = do
    pool <- TransactionC ask
    mask $ \restore -> do
      (conn, localPool) <- sendIO $ takeResource pool
      sendIO $ Pg.begin conn
      r <- restore (hdl (runReader conn t <$ ctx)) `onException` cleanUp conn pool localPool
      sendIO $ Pg.commit conn
      sendIO $ putResource localPool conn
      return r
    where
      cleanUp conn pool localPool = sendIO $ do
        Pg.rollback conn `E.catch` \(_ :: IOError) -> return ()
        destroyResource pool localPool conn
  alg hdl (R other) ctx = TransactionC (alg (runTransactionC . hdl) (R other) ctx)

runTransaction :: Pool Pg.Connection -> TransactionC m a -> m a
runTransaction pool = runReader pool . runTransactionC
