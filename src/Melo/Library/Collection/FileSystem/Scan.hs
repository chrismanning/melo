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
import Control.Concurrent.STM.Map qualified as SM
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
import Data.Maybe
import Data.Pool
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo qualified as Repo
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
            [] -> handleScanErrors files $ importTransaction files
            [cuefile] -> do
              $(logDebugIO) $ "Cue file found " <> show cuefile
              liftIO $
                handleAny (logShow files >=> \_ -> handleScanErrors files $ importTransaction files) $
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
            then liftIO $ handleScanErrors [p] $ importTransaction [p]
            else pure 0
    $(logInfoIO) $ show srcs <> " sources imported from path " <> show p
    pure ()
  where
    handleScanErrors :: (MonadIO m) => [FilePath] -> IO Int -> m Int
    handleScanErrors ps = liftIO . handleAny (logShow ps >=> \_ -> pure 0)
    importTransaction :: [FilePath] -> IO Int
    importTransaction files = do
      filterImports files >>= \case
        [] -> pure 0
        files -> do
          $(logDebugIO) $ T.pack $ "Importing " <> show files
          mfs <- catMaybes <$> mapM openMetadataFile'' files
          $(logDebugIO) $ T.pack $ "Opened " <> show (mfs <&> \mf -> mf.filePath)
          srcs <-
            runImport pool sess $
              importSources $
                V.fromList (FileSource ref <$> mfs)
          pure (V.length srcs)
    runImport pool sess =
        runConfigRepositoryPooledIO pool
        . runSourceRepositoryPooledIO pool
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
      runConfigRepositoryPooledIO pool $
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
    logShow :: [FilePath] -> SomeException -> IO ()
    logShow filePaths e = let !cause = displayException e in
      $(logErrorVIO ['filePaths]) $ "error during scan: " <> T.pack cause
    filterImports :: [FilePath] -> IO [FilePath]
    filterImports files | scanType == ScanAll = pure files
    filterImports files = do
      tz <- liftIO getCurrentTimeZone
      ss <- sourceMap files
      imports <- catMaybes <$> forM files \p ->
        case H.lookup (T.pack $ show $ fileUri p) ss of
          Just s -> do
            mtime <- utcToLocalTime tz <$> Dir.getModificationTime p
            if mtime > s.scanned
              then pure $ Just p
              else pure Nothing
          Nothing ->
            if isJust $ fileFactoryByExt p
              then pure $ Just p
              else pure Nothing
      pure imports
    sourceMap :: [FilePath] -> IO (H.HashMap T.Text SourceEntity)
    sourceMap files = runSourceRepositoryPooledIO pool do
      ss <- V.toList <$> getByUri (V.fromList $ fileUri <$> files)
      pure $ H.fromList $ (\s -> (s.source_uri, s)) <$> ss

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
    let conf = FS.defaultConfig { FS.confThreadingMode = ThreadPerWatch, FS.confOnHandlerException = handleException }
    void $
      liftIO $
        forkIO $
          forever $
            handle handleException $
              FS.withManagerConf conf \watchManager -> do
                events <- STM.atomically SM.empty
                let isLocked' e = do
                      !locks <- STM.atomically $ STM.readTVar watchState.locks
                      pure $ isLocked locks (pack e.eventPath)
                let handler e = unlessM (isLocked' e) do
                      tid <- forkIO do
                        $(logDebugIO) $ "FileSystem event received; waiting for 100ms: " <> show e
                        tid <- myThreadId
                        threadDelay 100000
                        latest <- STM.atomically do
                          SM.lookup e.eventPath events >>= \case
                            Just (e', tid') | e' == e && tid == tid' -> do
                              SM.delete e.eventPath events
                              pure True
                            _ -> pure False
                        $(logDebugIO) $ "Event is latest after 100ms: " <> show latest
                        when latest do
                          !locks <- STM.atomically $ STM.readTVar watchState.locks
                          handleEvent pool sess watchState ref locks e
                      STM.atomically $ SM.insert e.eventPath (e, tid) events
                stop <- FS.watchTree watchManager p (\e -> takeExtension e.eventPath `notElem` [".tmp", ".part"]) handler
                STM.atomically $ STM.modifyTVar' watchState.stoppers (H.insert ref stop)
                forever $ threadDelay 1000000
    where
      handleException (SomeException e) = $(logErrorIO) $ "Failure occured in filesystem watcher thread: " <> displayException e
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
    MonadIO m
  ) =>
  Pool Connection ->
  Wreq.Session ->
  CollectionWatchState ->
  CollectionRef ->
  Seq ByteString ->
  FS.Event ->
  m ()
handleEvent pool sess cws ref locks event = unless (isLocked locks (pack event.eventPath)) do
  case event of
    FS.Added p _ FS.IsDirectory -> do
      -- skip added directories to avoid reading half-written files
      -- the files should get their own events when finished writing (after debounced)
      $(logInfo) $ "directory added; skipping " <> T.pack (show p)
    FS.Added p _ FS.IsFile -> do
      $(logInfo) $ "file added; scanning " <> T.pack (show p)
      liftIO $ runParIO (scanPathIO pool sess cws ScanAll ref p)
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> T.pack (show p)
      liftIO $ runParIO (scanPathIO pool sess cws ScanNewOrModified ref p)
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> T.pack (show p)
      liftIO $ runParIO (scanPathIO pool sess cws ScanNewOrModified ref p)
      pure ()
    FS.WatchedDirectoryRemoved p _ _ -> do
      $(logInfo) $ "watched directory removed " <> T.pack (show p)
      let uri = fileUri p
      runSourceRepositoryPooledIO pool do
        refs <- getKeysByUriPrefix uri
        void $ Repo.delete @SourceEntity refs
    FS.Removed p _ isDir -> do
      let uri = fileUri p
      if isDir == FS.IsDirectory
        then do
          $(logInfo) $ "directory removed " <> T.pack (show p)
          runSourceRepositoryPooledIO pool do
            refs <- getKeysByUriPrefix uri
            void $ Repo.delete @SourceEntity refs
        else do
          $(logInfo) $ "file removed " <> T.pack (show p)
          runSourceRepositoryPooledIO pool do
            refs <- getKeysByUri (V.singleton uri)
            void $ Repo.delete @SourceEntity refs
    FS.Unknown p _ _ s ->
      $(logWarn) $ "unknown file system event on path " <> T.pack (show p) <> ": " <> T.pack s

isLocked :: Seq ByteString -> ByteString -> Bool
isLocked locks p = isJust $ Seq.findIndexL (\x -> isPrefixOf x p) locks
