{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Monad
  ( module Control.Monad,
    module Control.Monad.Base,
    module Control.Monad.Conc.Class,
    module Control.Monad.Extra,
    module Control.Monad.Primitive,
    module Control.Monad.Reader.Class,
    module Control.Monad.STM.Class,
    module Control.Monad.Trans,
    module Control.Monad.Trans.Control,
    module Control.Monad.Trans.Identity,
    module Control.Monad.Trans.Maybe,
    module Control.Monad.Trans.Reader,
    module Data.Foldable.Extra,
    AppData (..),
    AppDataReader (..),
    LocalAppDataReader (..),
    AppM,
    alterAppData,
    getAppData,
    deleteAppData,
    runAppM,
    forMaybeM,
    (<<|>>),
    once,
    runReaderT',
  )
where

import Control.Applicative as A
import Control.Concurrent.Classy
import Control.Monad
import Control.Monad.Base
import Control.Monad.Conc.Class hiding (catch)
import Control.Monad.Extra hiding
  ( allM,
    andM,
    anyM,
    concatForM,
    concatMapM,
    findM,
    firstJustM,
    fold1M,
    fold1M_,
    mapMaybeM,
    mconcatMapM,
    orM,
    partitionM,
  )
import Control.Monad.Primitive
import Control.Monad.Reader.Class
import Control.Monad.STM.Class
import Control.Monad.State.Strict
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Maybe hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Foldable.Extra
import Data.Proxy
import Data.TMap as TMap
import Data.Typeable
import Data.Vector qualified as V

forMaybeM :: (Monad m) => V.Vector a -> (a -> m (Maybe b)) -> m (V.Vector b)
forMaybeM = flip V.mapMaybeM

(<<|>>) :: (Monad m, Alternative f, Eq (f a)) => m (f a) -> m (f a) -> m (f a)
a <<|>> b =
  a >>= \case
    a'
      | a' == A.empty ->
          b >>= \case
            b' | b' == A.empty -> pure A.empty
            b' -> pure b'
    a' -> pure a'

once :: (MonadConc m) => m a -> m (m a)
once action = do
  mvar <- newEmptyMVar
  return $ do
    result <- takeMVar mvar
    case result of
      Just val -> return val
      Nothing -> do
        val <- action
        putMVar mvar (Just val)
        return val

runReaderT' :: r -> ReaderT r m a -> m a
runReaderT' = flip runReaderT

newtype AppData m = AppData
  { typeMap :: TVar (STM m) TMap
  }

type AppM b m = ReaderT (AppData b) m

runAppM :: (b ~ m, MonadConc m) => AppM b m a -> m a
runAppM m = do
  typeMapVar <- newTVarConc mempty
  let !appData = AppData typeMapVar
  runReaderT m appData

class (Monad m) => AppDataReader m where
  alterAppData' :: forall (a :: Type). (Typeable a) => (Maybe a -> Maybe a) -> m ()
  getAppData' :: forall (a :: Type). (Typeable a) => Proxy a -> m (Maybe a)
  putAppData :: forall (a :: Type). (Typeable a) => a -> m ()
  deleteAppData' :: forall (a :: Type). (Typeable a) => Proxy a -> m ()

instance (MonadIO m) => AppDataReader (AppM IO m) where
  alterAppData' alter = do
    mapVar <- asks (.typeMap)
    liftIO $ atomically $ modifyTVar' mapVar (TMap.alter alter)
  getAppData' _ = do
    mapVar <- asks (.typeMap)
    map <- liftIO $ readTVarConc mapVar
    pure $ TMap.lookup map
  putAppData a = do
    mapVar <- asks (.typeMap)
    liftIO $ atomically (modifyTVar' mapVar (TMap.insert a))
    pure ()
  deleteAppData' :: forall (a :: Type). (Typeable a) => Proxy a -> AppM IO m ()
  deleteAppData' _ = do
    mapVar <- asks (.typeMap)
    liftIO $ atomically (modifyTVar' mapVar (TMap.delete @a))
    pure ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    AppDataReader m
  ) =>
  AppDataReader (t m)
  where
  alterAppData' = lift . alterAppData
  getAppData' = lift . getAppData'
  putAppData = lift . putAppData
  deleteAppData' = lift . deleteAppData'

alterAppData :: forall (a :: Type) m. (AppDataReader m, Typeable a) => (Maybe a -> Maybe a) -> m ()
alterAppData = alterAppData' @m @a

getAppData :: forall (a :: Type) m. (AppDataReader m, Typeable a) => m (Maybe a)
getAppData = getAppData' (Proxy @a)

deleteAppData :: forall (a :: Type) m. (AppDataReader m, Typeable a) => m ()
deleteAppData = deleteAppData' (Proxy @a)

class (Monad m) => LocalAppDataReader m where
  localAppData :: forall (a :: Type) x. (Typeable a) => (Maybe a -> Maybe a) -> m x -> m x

instance (MonadIO m) => LocalAppDataReader (AppM IO m) where
  localAppData :: forall (a :: Type) x. (Typeable a) => (Maybe a -> Maybe a) -> AppM IO m x -> AppM IO m x
  localAppData f m = do
    mapVar <- asks (.typeMap)
    map <- liftIO $ readTVarConc mapVar
    let !map' = case f (TMap.lookup @a map) of
          Just a -> TMap.insert a map
          Nothing -> TMap.delete @a map
    map <- liftIO $ newTVarConc map'
    local (\appData -> appData {typeMap = map}) m

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MonadTransControl t,
    LocalAppDataReader m
  ) =>
  LocalAppDataReader (t m)
  where
  localAppData f m = liftWith (\run -> localAppData f (run m)) >>= restoreT . pure
