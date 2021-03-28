{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.WatchService where

import Control.Concurrent.Classy
import qualified Control.Concurrent.STM as C
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.HashMap.Strict as H
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import System.FSNotify (ThreadingMode (..))
import qualified System.FSNotify as FS
import System.FilePath

class Monad m => FileSystemWatchService m where
  startWatching :: CollectionRef -> FilePath -> m ()
  stopWatching :: CollectionRef -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    FileSystemWatchService m
  ) =>
  FileSystemWatchService (t m)
  where
  startWatching ref p = lift (startWatching ref p)
  stopWatching = lift . stopWatching

type CollectionWatchState = C.TVar (H.HashMap CollectionRef FS.StopListening)

newtype FileSystemWatchServiceIOT m a = FileSystemWatchServiceIOT
  { runFileSystemWatchServiceIOT :: ReaderT (Pool Connection, CollectionWatchState) m a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadCatch, MonadMask, MonadThrow, MonadTrans, MonadTransControl)

runFileSystemWatchServiceIO ::
  Pool Connection -> CollectionWatchState -> FileSystemWatchServiceIOT m a -> m a
runFileSystemWatchServiceIO pool watchState =
  flip runReaderT (pool, watchState) . runFileSystemWatchServiceIOT

instance
  ( MonadIO m,
    MonadMask m,
    MonadConc m,
    MonadBaseControl IO m,
    Logging m
  ) =>
  FileSystemWatchService (FileSystemWatchServiceIOT m)
  where
  startWatching ref p = FileSystemWatchServiceIOT $
    ReaderT $ \(pool, watchState) -> do
      $(logInfo) $ "starting to watch path " <> p
      liftBaseWith
        ( \runInBase ->
            void $
              fork $
                liftIO $
                  FS.withManagerConf (FS.defaultConfig {FS.confThreadingMode = ThreadPerEvent}) $ \watchManager -> do
                    stop <- FS.watchTree watchManager p (\e -> takeExtension (FS.eventPath e) `notElem` [".tmp", ".part"]) (void . runInBase . handleEvent pool ref)
                    C.atomically $ C.modifyTVar' watchState (H.insert ref stop)
                    forever $ threadDelay 1000000
        )
  stopWatching ref = FileSystemWatchServiceIOT $
    ReaderT $ \(_pool, watchState) -> do
      stoppers' <- liftIO $ C.atomically $ C.readTVar watchState
      case H.lookup ref stoppers' of
        Just stop -> liftIO stop
        Nothing -> pure ()

handleEvent ::
  ( Logging m,
    MonadIO m,
    MonadMask m,
    MonadConc m
  ) =>
  Pool Connection ->
  CollectionRef ->
  FS.Event ->
  m ()
handleEvent pool ref event = runFileSystemServiceIO' pool $
  case event of
    FS.Added p _ _ -> do
      $(logInfo) $ "file/directory added; scanning " <> p
      scanPath ref p
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> p
      scanPath ref p
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> p
      scanPath ref p
      pure ()
    FS.WatchedDirectoryRemoved p _ _ -> do
      $(logInfo) $ "watched directory removed " <> p
      let uri = fileUri p
      refs <- getSourceKeysByUriPrefix uri
      deleteSources refs
    FS.Removed p _ isDir -> do
      let uri = fileUri p
      if isDir == FS.IsDirectory
        then do
          $(logInfo) $ "directory removed " <> p
          refs <- getSourceKeysByUriPrefix uri
          deleteSources refs
        else do
          $(logInfo) $ "file removed " <> p
          refs <- getSourceKeysByUri [uri]
          deleteSources refs
    FS.Unknown p _ _ s ->
      $(logWarn) $ "unknown file system event on path " <> p <> ": " <> s
