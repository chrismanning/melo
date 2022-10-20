{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Service where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Par.IO
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool
import qualified Data.Text as T
import Data.Vector (Vector, singleton)
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
    MonadBaseControl IO m,
    MonadIO m,
    MonadConc m,
    Logging m
  ) =>
  CollectionService (CollectionServiceIOT m)
  where
  addCollection c@NewFilesystemCollection {..} = do
    $(logInfo) $ "Adding collection " <> name
    $(logDebug) $ "Adding collection " <> show c
    cs <- Repo.insert (singleton c)
    case firstOf traverse cs of
      Just CollectionTable {..} -> do
        pool <- ask
        fork $ liftIO $ runParIO $ scanPathIO pool ScanAll id (T.unpack rootPath)
        when watch $ startWatching id (T.unpack rootPath)
        pure id
      Nothing -> error "unexpected insertCollections result"
  rescanCollection ref@(CollectionRef id) = do
    firstOf traverse <$> getCollectionsByKey (singleton ref) >>= \case
      Just CollectionTable {id, root_uri} ->
        case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            pool <- ask
            $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
            liftIO $ runParIO $ scanPathIO pool ScanNewOrModified ref rootPath
          Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
      Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
    pure ()
  deleteCollection ref = do
    stopWatching ref
    delete (singleton ref)
    pure ()

getCollectionsByKey :: (CollectionRepository m) => Vector CollectionRef -> m (Vector Collection)
getCollectionsByKey = getByKey
