{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Collection.Aggregate where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens (firstOf)
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
import Melo.Library.Collection.FileSystem.Scan
import Melo.Library.Collection.FileSystem.Watcher
import Melo.Library.Collection.Repo as Repo
import Melo.Library.Collection.Types
import Network.Wreq.Session qualified as Wreq

class Monad m => CollectionAggregate m where
  addCollection :: NewCollection -> m CollectionRef
  deleteCollection :: CollectionRef -> m (Maybe CollectionRef)
  rescanCollection :: CollectionRef -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    CollectionAggregate m
  ) =>
  CollectionAggregate (t m)
  where
  addCollection = lift . addCollection
  deleteCollection = lift . deleteCollection
  rescanCollection = lift . rescanCollection

newtype CollectionAggregateIOT m a = CollectionAggregateIOT
  { runCollectionAggregateIOT :: ReaderT (Pool Connection, Wreq.Session) m a
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
      MonadReader (Pool Connection, Wreq.Session),
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

runCollectionAggregateIO :: Pool Connection -> Wreq.Session -> CollectionAggregateIOT m a -> m a
runCollectionAggregateIO pool sess = flip runReaderT (pool, sess) . runCollectionAggregateIOT

instance
  ( CollectionRepository m,
    FileSystemWatcher m,
    MonadBaseControl IO m,
    MonadIO m,
    MonadConc m,
    Logging m
  ) =>
  CollectionAggregate (CollectionAggregateIOT m)
  where
  addCollection c@NewFilesystemCollection {..} = do
    $(logInfo) $ "Adding collection " <> name
    $(logDebug) $ "Adding collection " <> show c
    Repo.insertSingle c >>= \case
      Just CollectionTable {..} -> do
        (pool, sess) <- ask
        fork $ liftIO $ runParIO $ scanPathIO pool sess ScanAll id (T.unpack rootPath)
        when watch $ startWatching id (T.unpack rootPath)
        pure id
      Nothing -> error "unexpected insertCollections result"
  rescanCollection ref@(CollectionRef id) = do
    getSingle ref >>= \case
      Just CollectionTable {id, root_uri} ->
        case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            (pool, sess) <- ask
            $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
            liftIO $ runParIO $ scanPathIO pool sess ScanNewOrModified ref rootPath
          Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
      Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
    pure ()
  deleteCollection ref = do
    stopWatching ref
    firstOf traverse <$> delete (singleton ref)

getCollectionsByKey :: (CollectionRepository m) => Vector CollectionRef -> m (Vector CollectionEntity)
getCollectionsByKey = getByKey