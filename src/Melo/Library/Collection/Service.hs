{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Service where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Hasql.Connection
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Database.Repo as Repo
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo as Repo
import Melo.Library.Collection.Types

class Monad m => CollectionService m where
  addCollection :: NewCollection -> m CollectionRef
  deleteCollection :: CollectionRef -> m ()
  rescanCollection :: CollectionRef -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    CollectionService m
  ) =>
  CollectionService (t m)
  where
  addCollection = lift . addCollection
  deleteCollection = lift . deleteCollection
  rescanCollection = lift . rescanCollection

newtype CollectionServiceIOT m a = CollectionServiceIOT
  { runCollectionServiceIOT :: ReaderT (Pool Connection) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadCatch,
      MonadConc,
      MonadMask,
      MonadParallel,
      MonadThrow,
      MonadReader (Pool Connection),
      MonadTrans,
      MonadTransControl
    )

runCollectionServiceIO :: Pool Connection -> CollectionServiceIOT m a -> m a
runCollectionServiceIO pool = flip runReaderT pool . runCollectionServiceIOT

instance
  ( CollectionRepository m,
    FileSystemService m,
    FileSystemWatchService m,
    MonadConc m,
    MonadParallel m,
    MonadIO m,
    Logging m
  ) =>
  CollectionService (CollectionServiceIOT m)
  where
  addCollection c@NewFilesystemCollection {..} = do
    pool <- ask
    $(logInfo) $ "Adding collection " <> name
    $(logDebug) $ "Adding collection " <> show c
    cs <- Repo.insert [c]
    case cs of
      [CollectionTable {..}] -> do
        forkFileSystemServiceIO pool $ scanPath id (T.unpack rootPath)
        when watch $ startWatching id (T.unpack rootPath)
        pure id
      _otherwise -> error "unexpected insertCollections result"
  rescanCollection ref@(CollectionRef id) = do
    listToMaybe <$> getCollectionsByKey [ref] >>= \case
      Just CollectionTable {id, root_uri} ->
        case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
            scanPath ref rootPath >> pure ()
          Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
      Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
    pure ()
  deleteCollection ref = do
    stopWatching ref
    delete [ref]
    pure ()

getCollectionsByKey :: (CollectionRepository m) => [CollectionRef] -> m [Collection]
getCollectionsByKey = getByKey
