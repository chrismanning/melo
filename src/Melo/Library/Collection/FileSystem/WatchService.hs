{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.WatchService where

import Control.Concurrent.Classy
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack)
import Data.HashMap.Strict as H
import Data.Pool
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Database.Repo as Repo
import Melo.Database.Transaction
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import System.FSNotify (ThreadingMode (..))
import System.FSNotify qualified as FS
import System.FilePath

class Monad m => FileSystemWatchService m where
  startWatching :: CollectionRef -> FilePath -> m ()
  stopWatching :: CollectionRef -> m ()
  lockPath :: FilePath -> ResourceT m ()

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
  lockPath p = lockPath p

data CollectionWatchState = CollectionWatchState
  { stoppers :: STM.TVar (H.HashMap CollectionRef FS.StopListening),
    locks :: STM.TVar (V.Vector ByteString)
  }

emptyWatchState :: IO CollectionWatchState
emptyWatchState = do
  stoppers <- atomically $ newTVar H.empty
  locks <- atomically $ newTVar V.empty
  pure CollectionWatchState {
    ..
  }

newtype FileSystemWatchServiceIOT m a = FileSystemWatchServiceIOT
  { runFileSystemWatchServiceIOT :: ReaderT (Pool Connection, CollectionWatchState) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader (Pool Connection, CollectionWatchState),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadParallel,
      MonadBase b,
      MonadBaseControl b,
      MonadUnliftIO
    )

runFileSystemWatchServiceIO ::
  Pool Connection -> CollectionWatchState -> FileSystemWatchServiceIOT m a -> m a
runFileSystemWatchServiceIO pool watchState =
  flip runReaderT (pool, watchState) . runFileSystemWatchServiceIOT

instance
  ( MonadIO m,
    MonadConc m,
    MonadBaseControl IO m,
    Logging m
  ) =>
  FileSystemWatchService (FileSystemWatchServiceIOT m)
  where
  startWatching ref p = do
    (pool, watchState) <- ask
    $(logInfo) $ "starting to watch path " <> p
    liftBaseWith
      ( \runInBase ->
          void $
            fork $
              liftIO $
                FS.withManagerConf (FS.defaultConfig {FS.confThreadingMode = ThreadPerWatch}) $ \watchManager -> do
                  let handler e = do
                        locks <- STM.atomically $ STM.readTVar watchState.locks
                        void $ runInBase $ handleEvent pool ref locks e
                  stop <- FS.watchTree watchManager p (\e -> takeExtension (FS.eventPath e) `notElem` [".tmp", ".part"]) handler
                  STM.atomically $ STM.modifyTVar' watchState.stoppers (H.insert ref stop)
                  forever $ threadDelay 1000000
      )
  stopWatching ref = do
    (_pool, watchState) <- ask
    stoppers' <- liftIO $ STM.atomically $ STM.readTVar watchState.stoppers
    case H.lookup ref stoppers' of
      Just stop -> liftIO stop
      Nothing -> pure ()
  lockPath p = do
    (_pool, watchState) <- ask
    let bytes = pack p
    liftIO $ STM.atomically $ STM.modifyTVar' watchState.locks (addLock bytes)
    void $ register (liftIO $ STM.atomically $ STM.modifyTVar' watchState.locks (rmLock bytes))
    where
      addLock = V.cons
      rmLock p' = V.filter (/= p')

handleEvent ::
  ( Logging m,
    MonadIO m,
    MonadConc m
  ) =>
  Pool Connection ->
  CollectionRef ->
  V.Vector ByteString ->
  FS.Event ->
  m ()
handleEvent pool ref locks event = unless (isLocked (pack event.eventPath)) do
  case event of
    FS.Added p _ _ -> do
      $(logInfo) $ "file/directory added; scanning " <> p
      liftIO $ runFileSystemServiceIO' pool (scanPath ref p)
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> p
      liftIO $ runFileSystemServiceIO' pool (scanPath ref p)
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> p
      liftIO $ runFileSystemServiceIO' pool (scanPath ref p)
      pure ()
    FS.WatchedDirectoryRemoved p _ _ -> do
      $(logInfo) $ "watched directory removed " <> p
      let uri = fileUri p
      withTransaction pool runSourceRepositoryIO do
        refs <- getKeysByUriPrefix uri
        Repo.delete refs
    FS.Removed p _ isDir -> do
      let uri = fileUri p
      if isDir == FS.IsDirectory
        then do
          $(logInfo) $ "directory removed " <> p
          withTransaction pool runSourceRepositoryIO do
            refs <- getKeysByUriPrefix uri
            Repo.delete refs
        else do
          $(logInfo) $ "file removed " <> p
          withTransaction pool runSourceRepositoryIO do
            refs <- getKeysByUri (V.singleton uri)
            Repo.delete refs
    FS.Unknown p _ _ s ->
      $(logWarn) $ "unknown file system event on path " <> p <> ": " <> s
  where
    isLocked p = V.any (\x -> isPrefixOf x p) locks
