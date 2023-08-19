module Melo.Common.Monad.Par where

import Control.Concurrent.Classy.STM
import Control.Monad.IO.Class
import Control.Monad.Par.Class
import Control.Monad.Par.IO
import Data.Kind
import Data.Proxy
import Data.TMap qualified as TMap
import Data.Typeable
import Melo.Common.Monad
  ( AppData (..),
    AppDataReader (..),
    AppM,
    MonadConc (..),
    MonadReader (..),
    ReaderT (..),
    asks,
  )

newtype ParAppM a = ParAppM (AppM IO ParIO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (AppData IO))

runParAppM :: MonadIO m => AppData IO -> ParAppM a -> m a
runParAppM appData (ParAppM (ReaderT m)) = liftIO $ runParIO $ m appData

instance ParFuture IVar ParAppM where
  get f = ParAppM (ReaderT \_appData -> get f)
  spawn_ p = do
    r <- new
    fork (p >>= put_ r)
    return r
  spawn p = do
    r <- new
    fork (p >>= put r)
    return r

instance ParIVar IVar ParAppM where
  fork (ParAppM (ReaderT m)) = ParAppM do
    appData <- ask
    liftIO $ runParIO $ fork (m appData)
  new = ParAppM do
    liftIO $ runParIO new
  put_ i a = ParAppM do
    liftIO $ runParIO $ put_ i a

instance AppDataReader ParAppM where
  getAppData' _ = ParAppM do
    mapVar <- asks (.typeMap)
    map <- liftIO $ readTVarConc mapVar
    pure $ TMap.lookup map
  putAppData a = ParAppM do
    mapVar <- asks (.typeMap)
    liftIO $ atomically (modifyTVar' mapVar (TMap.insert a))
    pure ()
  deleteAppData' :: forall (a :: Type). Typeable a => Proxy a -> ParAppM ()
  deleteAppData' _ = ParAppM do
    mapVar <- asks (.typeMap)
    liftIO $ atomically (modifyTVar' mapVar (TMap.delete @a))
    pure ()
