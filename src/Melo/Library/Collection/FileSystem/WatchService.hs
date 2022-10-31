{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Collection.FileSystem.WatchService where

import Control.Concurrent
import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Par.IO
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack)
import Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Pool
import Data.Sequence qualified as Seq
import Data.Sequence (Seq (..))
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Database.Repo as Repo
import Melo.Database.Transaction
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Network.Wreq.Session as Wreq
import System.FSNotify (ThreadingMode (..))
import System.FSNotify qualified as FS
import System.FilePath

class Monad m => FileSystemWatchService m where
  startWatching :: CollectionRef -> FilePath -> m ()
  stopWatching :: CollectionRef -> m ()
  lockPathsDuring :: NonEmpty FilePath -> m a -> m a

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
  lockPathsDuring p m = liftWith (\run -> lockPathsDuring p (run m)) >>= restoreT . pure

data CollectionWatchState = CollectionWatchState
  { stoppers :: STM.TVar (H.HashMap CollectionRef FS.StopListening),
    locks :: STM.TVar (Seq ByteString)
  }

emptyWatchState :: IO CollectionWatchState
emptyWatchState = STM.atomically $
  CollectionWatchState <$> STM.newTVar H.empty <*> STM.newTVar Seq.empty

newtype FileSystemWatchServiceIOT m a = FileSystemWatchServiceIOT
  { runFileSystemWatchServiceIOT :: ReaderT (Pool Connection, CollectionWatchState, Wreq.Session) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader (Pool Connection, CollectionWatchState, Wreq.Session),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadBase b,
      MonadBaseControl b,
      PrimMonad
    )

runFileSystemWatchServiceIO ::
  Pool Connection -> CollectionWatchState -> Wreq.Session -> FileSystemWatchServiceIOT m a -> m a
runFileSystemWatchServiceIO pool watchState sess =
  flip runReaderT (pool, watchState, sess) . runFileSystemWatchServiceIOT

instance
  ( MonadIO m,
    MonadMask m,
    MonadBaseControl IO m,
    Logging m
  ) =>
  FileSystemWatchService (FileSystemWatchServiceIOT m)
  where
  startWatching ref p = do
    (pool, watchState, sess) <- ask
    $(logInfo) $ "starting to watch path " <> p
    liftBaseWith
      ( \runInBase ->
          void $
            liftIO $
              forkIO $
                FS.withManagerConf (FS.defaultConfig {FS.confThreadingMode = ThreadPerWatch}) $ \watchManager -> do
                  let handler e = do
                        locks <- STM.atomically $ STM.readTVar watchState.locks
                        void $ runInBase $ handleEvent pool sess ref locks e
                  stop <- FS.watchTree watchManager p (\e -> takeExtension (FS.eventPath e) `notElem` [".tmp", ".part"]) handler
                  STM.atomically $ STM.modifyTVar' watchState.stoppers (H.insert ref stop)
                  forever $ threadDelay 1000000
      )
  stopWatching ref = do
    (_pool, watchState, _sess) <- ask
    stoppers' <- liftIO $ STM.atomically $ STM.readTVar watchState.stoppers
    case H.lookup ref stoppers' of
      Just stop -> liftIO stop
      Nothing -> pure ()
  lockPathsDuring ps m = do
    (_pool, watchState, _sess) <- ask
    let packedPaths = Seq.fromList $ NE.toList $ pack <$> ps
    bracket
      (liftIO $ do
        STM.atomically $ STM.modifyTVar' watchState.locks (lockPaths packedPaths)
        $(logInfoIO) $ "Unwatching paths " <> show packedPaths
      )
      (\_ -> liftIO $ do
        $(logInfoIO) $ "Re-watching paths " <> show packedPaths
        STM.atomically $ STM.modifyTVar' watchState.locks (unlockPaths packedPaths)
      )
      (const m)
    where
      lockPaths = (Seq.><)
      unlockPaths packedPaths =
        Seq.filter (\lock -> isNothing $ Seq.elemIndexL lock packedPaths)

handleEvent ::
  ( Logging m,
    MonadMask m,
    MonadIO m
  ) =>
  Pool Connection ->
  Wreq.Session ->
  CollectionRef ->
  Seq ByteString ->
  FS.Event ->
  m ()
handleEvent pool sess ref locks event = unless (isLocked (pack event.eventPath)) do
  case event of
    FS.Added p _ _ -> do
      $(logInfo) $ "file/directory added; scanning " <> p
      liftIO $ runParIO (scanPathIO pool sess ScanAll ref p)
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> p
      liftIO $ runParIO (scanPathIO pool sess ScanNewOrModified ref p)
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> p
      liftIO $ runParIO (scanPathIO pool sess ScanNewOrModified ref p)
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
    isLocked p = isJust $ Seq.findIndexL (\x -> isPrefixOf x p) locks
