{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.WatchService where

import Control.Concurrent
import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
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
  lockPathDuring :: FilePath -> m a -> m a

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MonadTransControl t,
    FileSystemWatchService m
  ) =>
  FileSystemWatchService (t m)
  where
  startWatching ref p = lift (startWatching ref p)
  stopWatching = lift . stopWatching
  lockPathDuring p m = liftWith (\run -> lockPathDuring p (run m)) >>= restoreT . pure

data CollectionWatchState = CollectionWatchState
  { stoppers :: STM.TVar (H.HashMap CollectionRef FS.StopListening),
    locks :: STM.TVar (V.Vector ByteString)
  }

emptyWatchState :: IO CollectionWatchState
emptyWatchState = do
  stoppers <- STM.atomically $ STM.newTVar H.empty
  locks <- STM.atomically $ STM.newTVar V.empty
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
      MonadBase b,
      MonadBaseControl b
    )

runFileSystemWatchServiceIO ::
  Pool Connection -> CollectionWatchState -> FileSystemWatchServiceIOT m a -> m a
runFileSystemWatchServiceIO pool watchState =
  flip runReaderT (pool, watchState) . runFileSystemWatchServiceIOT

instance
  ( MonadIO m,
    MonadMask m,
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
            liftIO $
              forkIO $
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
  lockPathDuring p m = do
    (_pool, watchState) <- ask
    let bytes = pack p
    bracket
      (liftIO $ STM.atomically $ STM.modifyTVar' watchState.locks (addLock bytes))
      (\_ -> liftIO $ STM.atomically $ STM.modifyTVar' watchState.locks (rmLock bytes))
      (const m)
    where
      addLock = V.cons
      rmLock p' = V.filter (/= p')

handleEvent ::
  ( Logging m,
    MonadMask m,
    MonadIO m
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
