{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Scan
  ( scanPathIO,
    ScanType (..),
    runFileSystemWatcherIO,
    FileSystemWatcherIOT (..),
    CollectionWatchState,
    emptyWatchState,
  )
where

import Control.Concurrent
import Control.Concurrent.Classy (MonadConc)
import Control.Concurrent.STM qualified as STM
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Extra
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.ByteString.Char8 (ByteString, isPrefixOf, pack)
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Pool
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo qualified as Repo
import Melo.Database.Transaction
import Melo.Format.Error qualified as F
import Melo.Format.Metadata (MetadataFile (..), fileFactoryByExt)
import Melo.Library.Release.Aggregate
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Types
import Melo.Library.Source.Aggregate
import Melo.Library.Source.Cue
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
  ( NewImportSource (..),
    SourceEntity,
    SourceTable (..),
  )
import Melo.Library.Track.Aggregate
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.Wreq.Session qualified as Wreq
import System.FSNotify (ThreadingMode (..))
import System.FSNotify qualified as FS
import System.FilePath
import UnliftIO.Directory qualified as Dir

data ScanType = ScanNewOrModified | ScanAll
  deriving (Eq)

scanPathIO ::
  Pool Connection ->
  Wreq.Session ->
  CollectionWatchState ->
  ScanType ->
  CollectionRef ->
  FilePath ->
  ParIO ()
scanPathIO pool sess cws scanType ref p' =
  do
    p <- Dir.canonicalizePath p'
    $(logInfoIO) $ "Scanning " <> show p
    if scanType == ScanNewOrModified
      then $(logDebugIO) $ "Looking for updated/new files in " <> show p
      else $(logDebugIO) $ "Looking for files in " <> show p
    isDir <- Dir.doesDirectoryExist p
    isFile <- Dir.doesFileExist p
    srcs <-
      if isDir
        then do
          $(logDebugIO) $ p <> " is directory; recursing..."
          entries <- Dir.listDirectory p
          dirs <- filterM Dir.doesDirectoryExist ((p </>) <$> entries)

          _ <- parMapM (scanPathIO pool sess cws scanType ref) dirs

          files <- filterM Dir.doesFileExist ((p </>) <$> entries)
          let cuefiles = filter ((== ".cue") . takeExtension) files
          case cuefiles of
            [] -> handleScanErrors $ importTransaction files
            [cuefile] -> do
              $(logDebugIO) $ "Cue file found " <> show cuefile
              liftIO $
                handle (logShow >=> \_ -> handleScanErrors $ importTransaction files) $
                  V.length
                    <$> runImport
                      pool
                      sess
                      ( openCueFile cuefile <&> (CueFileImportSource ref <$>) >>= importSources
                      )
            _ -> do
              $(logWarnIO) $ "Multiple cue file found in " <> show p <> "; skipping..."
              pure 0
        else
          if isFile
            then
              liftIO $
                ifM
                  (shouldImport [p])
                  (handleScanErrors $ importTransaction [p])
                  (pure 0)
            else pure 0
    $(logInfoIO) $ show srcs <> " sources imported from path " <> show p
    pure ()
  where
    handleScanErrors :: (MonadIO m) => IO Int -> m Int
    handleScanErrors = liftIO . handle (logShow >=> \_ -> pure 0)
    importTransaction :: [FilePath] -> IO Int
    importTransaction files = do
      ifM
        (shouldImport files)
        ( do
            $(logDebugIO) $ "Importing " <> show files
            mfs <- catMaybes <$> mapM openMetadataFile'' files
            $(logDebugIO) $ "Opened " <> show (mfs <&> \mf -> mf.filePath)
            srcs <-
              runImport pool sess $
                importSources $
                  V.fromList (FileSource ref <$> mfs)
            pure (V.length srcs)
        )
        (pure 0)
    runImport pool sess =
      runSourceRepositoryPooledIO pool
        . runArtistRepositoryPooledIO pool
        . runArtistNameRepositoryPooledIO pool
        . runReleaseArtistNameRepositoryPooledIO pool
        . runReleaseRepositoryPooledIO pool
        . runTrackRepositoryPooledIO pool
        . runTrackArtistNameRepositoryPooledIO pool
        . MB.runMusicBrainzServiceUnlimitedIO sess
        . runTagMappingRepositoryPooledIO pool
        . MB.runCachingMusicBrainzService
        . runTagMappingAggregate
        . runFileSystemWatcherIO pool cws sess
        . runMetadataAggregateIO
        . runArtistAggregateIOT
        . runTrackAggregateIOT
        . runReleaseAggregateIOT
        . runSourceAggregateIOT
    openMetadataFile'' p =
      runFileSystemWatcherIO pool cws sess $
        runMetadataAggregateIO $
          openMetadataFileByExt p >>= \case
            Right mf -> pure $ Just mf
            Left e@F.UnknownFormat -> do
              $(logWarnIO) $ "Could not open by extension " <> p <> ": " <> show e
              openMetadataFile p >>= \case
                Left e -> do
                  $(logErrorIO) $ "Could not open " <> p <> ": " <> show e
                  pure Nothing
                Right mf -> pure $ Just mf
            Left e -> do
              $(logErrorIO) $ "Could not open by extension " <> p <> ": " <> show e
              pure Nothing
    logShow :: SomeException -> IO ()
    logShow e = $(logErrorIO) $ "error during scan: " <> displayException e
    shouldImport :: [FilePath] -> IO Bool
    shouldImport _ | scanType == ScanAll = pure True
    shouldImport files = handle (logShow >=> \_ -> pure False) do
      tz <- liftIO getCurrentTimeZone
      ss <- sourceMap files
      updated <- forM files $ \p ->
        case M.lookup (T.pack $ show $ fileUri p) ss of
          Just s -> do
            mtime <- utcToLocalTime tz <$> Dir.getModificationTime p
            if mtime > s.scanned
              then do
                $(logInfoIO) $ "Importing updated path " <> show p
                pure True
              else pure False
          Nothing ->
            if isJust $ fileFactoryByExt p
              then do
                $(logInfoIO) $ "Importing new path " <> show p
                pure True
              else pure False
      pure $ any (== True) updated
    sourceMap :: [FilePath] -> IO (M.Map T.Text SourceEntity)
    sourceMap files = runSourceRepositoryPooledIO pool do
      ss <- V.toList <$> getByUri (V.fromList $ fileUri <$> files)
      pure $ M.fromList $ (\s -> (s.source_uri, s)) <$> ss

data CollectionWatchState = CollectionWatchState
  { stoppers :: STM.TVar (H.HashMap CollectionRef FS.StopListening),
    locks :: STM.TVar (Seq ByteString)
  }

emptyWatchState :: IO CollectionWatchState
emptyWatchState =
  STM.atomically $
    CollectionWatchState <$> STM.newTVar H.empty <*> STM.newTVar Seq.empty

newtype FileSystemWatcherIOT m a = FileSystemWatcherIOT
  { runFileSystemWatcherIOT :: ReaderT (Pool Connection, CollectionWatchState, Wreq.Session) m a
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

runFileSystemWatcherIO ::
  Pool Connection -> CollectionWatchState -> Wreq.Session -> FileSystemWatcherIOT m a -> m a
runFileSystemWatcherIO pool watchState sess =
  flip runReaderT (pool, watchState, sess) . runFileSystemWatcherIOT

instance
  ( MonadIO m,
    MonadMask m,
    MonadBaseControl IO m,
    Logging m
  ) =>
  FileSystemWatcher (FileSystemWatcherIOT m)
  where
  startWatching ref p = do
    (pool, watchState, sess) <- ask
    $(logInfo) $ "starting to watch path " <> p
    void $
      liftIO $
        forkIO $
          handle (\(SomeException e) -> $(logErrorIO) $ "Failure occured in filesystem watcher thread: " <> displayException e) $
            FS.withManagerConf (FS.defaultConfig {FS.confThreadingMode = ThreadPerWatch}) $ \watchManager -> do
              let handler e = do
                    locks <- STM.atomically $ STM.readTVar watchState.locks
                    void $ handleEvent pool sess watchState ref locks e
              stop <- FS.watchTree watchManager p (\e -> takeExtension e.eventPath `notElem` [".tmp", ".part"]) handler
              STM.atomically $ STM.modifyTVar' watchState.stoppers (H.insert ref stop)
              forever $ threadDelay 1000000
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
      ( liftIO $ do
          STM.atomically $ STM.modifyTVar' watchState.locks (lockPaths packedPaths)
          $(logInfoIO) $ "Unwatching paths " <> show packedPaths
      )
      ( \_ -> liftIO $ do
          threadDelay 10000
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
  CollectionWatchState ->
  CollectionRef ->
  Seq ByteString ->
  FS.Event ->
  m ()
handleEvent pool sess cws ref locks event = unless (isLocked (pack event.eventPath)) do
  case event of
    FS.Added p _ _ -> do
      $(logInfo) $ "file/directory added; scanning " <> p
      liftIO $ runParIO (scanPathIO pool sess cws ScanAll ref p)
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> p
      liftIO $ runParIO (scanPathIO pool sess cws ScanNewOrModified ref p)
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> p
      liftIO $ runParIO (scanPathIO pool sess cws ScanNewOrModified ref p)
      pure ()
    FS.WatchedDirectoryRemoved p _ _ -> do
      $(logInfo) $ "watched directory removed " <> p
      let uri = fileUri p
      withTransaction pool runSourceRepositoryIO do
        refs <- getKeysByUriPrefix uri
        void $ Repo.delete @SourceEntity refs
    FS.Removed p _ isDir -> do
      let uri = fileUri p
      if isDir == FS.IsDirectory
        then do
          $(logInfo) $ "directory removed " <> p
          withTransaction pool runSourceRepositoryIO do
            refs <- getKeysByUriPrefix uri
            void $ Repo.delete @SourceEntity refs
        else do
          $(logInfo) $ "file removed " <> p
          withTransaction pool runSourceRepositoryIO do
            refs <- getKeysByUri (V.singleton uri)
            void $ Repo.delete @SourceEntity refs
    FS.Unknown p _ _ s ->
      $(logWarn) $ "unknown file system event on path " <> p <> ": " <> s
  where
    isLocked p = isJust $ Seq.findIndexL (\x -> isPrefixOf x p) locks
