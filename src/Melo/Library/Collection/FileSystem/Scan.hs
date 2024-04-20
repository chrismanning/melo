{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Library.Collection.FileSystem.Scan
  ( scanPathIO,
    ScanType (..),
  )
where

import Control.Concurrent.Chan.Unagi.Bounded
import Control.Concurrent.Classy (modifyTVar',)
import Control.Concurrent.STM.Map qualified as SM
import Control.Monad.Extra
import Control.Monad.Par.Class hiding (fork)
import Control.Monad.Par.IO
import Control.Monad.Trans.Cont
import Data.HashMap.Strict qualified as H
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Typeable
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing ()
import Melo.Common.Uri
import Melo.Database.Repo qualified as Repo
import Melo.Format.Error qualified as F
import Melo.Format.Metadata (MetadataFile (..), fileFactoryByExt)
import Melo.Library.Collection.Types
import Melo.Library.Source.Aggregate
import Melo.Library.Source.Cue
import Melo.Library.Source.Repo as SrcRepo
import Melo.Library.Source.Types
  ( NewImportSource (..),
    SourceEntity,
    SourceTable (..),
  )
import Melo.Metadata.Aggregate
import Streaming.Prelude qualified as S
import System.FSNotify (ThreadingMode (..))
import System.FSNotify qualified as FS
import System.FilePath
import UnliftIO.Directory qualified as Dir
import Unsafe.Coerce

data ScanType = ScanNewOrModified | ScanAll
  deriving (Eq)

scanPathIO ::
  ScanType ->
  CollectionRef ->
  FilePath ->
  AppM IO IO ()
scanPathIO scanType ref p = do
  appData <- ask
  liftIO $ runParIO (scanPath appData scanType ref p)

scanPath ::
  AppData IO ->
  ScanType ->
  CollectionRef ->
  FilePath ->
  ParIO ()
scanPath appData scanType ref p' =
  do
    p <- Dir.canonicalizePath p'
    $(logInfoIO) $ "Scanning " <> showt p
    if scanType == ScanNewOrModified
      then $(logDebugIO) $ "Looking for updated/new files in " <> showt p
      else $(logDebugIO) $ "Looking for files in " <> showt p
    isDir <- Dir.doesDirectoryExist p
    isFile <- Dir.doesFileExist p
    srcs <- do
      (inChan, outChan) <- liftIO $ newChan 1
      runContT'
        (liftIO . writeChan inChan)
        if isDir
          then do
            $(logDebugIO) $ showt p <> " is directory; recursing..."
            entries <- streamDirEntriesIO p <&> S.map (\e -> e & #name %~ (p </>))
            files <-
              lift $
                S.partition (\e -> e.type' == DirEntryDir) entries
                  & S.mapM_ (\e -> lift $ spawn_ (scanPath appData scanType ref e.name))
                  & S.map (.name)
                  & S.toList_
            let cuefiles = filter ((== ".cue") . takeExtension) files
            lift $ runAppM case cuefiles of
              [] -> handleScanErrors files $ importTransaction files
              [cuefile] -> do
                $(logDebugIO) $ "Cue file found " <> showt cuefile
                handleAny (logShow files >=> \_ -> handleScanErrors files $ importTransaction files) $
                  V.length
                    <$> (openCueFile cuefile <&> (CueFileImportSource ref <$>) >>= importSources)
              _ -> do
                $(logWarnIO) $ "Multiple cue file found in " <> showt p <> "; skipping..."
                pure 0
          else
            if isFile
              then lift $ runAppM $ handleScanErrors [p] $ importTransaction [p]
              else pure 0
      liftIO $ readChan outChan
    $(logInfoIO) $ showt srcs <> " sources imported from path " <> showt p
    pure ()
  where
    runContT' = flip runContT
    runAppM :: AppM IO IO a -> ParIO a
    runAppM m = do
      liftIO $ runReaderT' appData m
    handleScanErrors ps = handleAny (logShow ps >=> \_ -> pure 0)
    importTransaction :: [FilePath] -> AppM IO IO Int
    importTransaction files =
      filterImports files >>= \case
        [] -> pure 0
        files -> do
          $(logDebug) $ "Importing " <> showt files
          mfs <- catMaybes <$> mapM openMetadataFile'' files
          $(logDebug) $ "Opened " <> showt (mfs <&> \mf -> mf.filePath)
          srcs <- importSources $ V.fromList (FileSource ref <$> mfs)
          pure (V.length srcs)
    openMetadataFile'' :: FilePath -> AppM IO IO (Maybe MetadataFile)
    openMetadataFile'' p =
      openMetadataFileByExt p >>= \case
        Right mf -> pure $ Just mf
        Left e@F.UnknownFormat -> do
          let cause = displayException e
          $(logWarnV ['cause]) $ "Could not open by extension " <> showt p
          openMetadataFile p >>= \case
            Left e -> do
              let cause = displayException e
              $(logErrorV ['cause]) $ "Could not open " <> showt p
              pure Nothing
            Right mf -> pure $ Just mf
        Left e -> do
          let cause = displayException e
          $(logErrorVIO ['cause]) $ "Could not open by extension " <> showt p
          pure Nothing
    logShow :: (Logging m) => [FilePath] -> SomeException -> m ()
    logShow filePaths e =
      let !cause = displayException e
       in $(logErrorV ['filePaths, 'cause]) "error during scan"
    filterImports :: [FilePath] -> AppM IO IO [FilePath]
    filterImports files | scanType == ScanAll = pure files
    filterImports files = do
      tz <- liftIO getCurrentTimeZone
      ss <- sourceMap files
      imports <-
        catMaybes <$> forM files \p ->
          case H.lookup (showt $ fileUri p) ss of
            Just s -> do
              mtime <- utcToLocalTime tz <$> Dir.getModificationTime p
              if mtime > s.scanned
                then pure $ Just p
                else pure Nothing
            Nothing ->
              if isn't _Nothing $ fileFactoryByExt p
                then pure $ Just p
                else pure Nothing
      pure imports
    sourceMap :: [FilePath] -> AppM IO IO (H.HashMap Text SourceEntity)
    sourceMap files = do
      ss <- V.toList <$> SrcRepo.getByUri (V.fromList $ fileUri <$> files)
      pure $ H.fromList $ (\s -> (s.source_uri, s)) <$> ss

instance
  ( Monad m,
    MonadIO m,
    MonadConc m,
    Typeable m,
    Logging m
  ) =>
  FileSystemWatcher (AppM IO m)
  where
  startWatching ref p = do
    $(logInfo) $ "Starting to watch path " <> showt p
    let conf = FS.defaultConfig {FS.confThreadingMode = ThreadPerWatch, FS.confOnHandlerException = handleException}
    void $
      fork $
        forever $
          handle handleException do
            watchState <- getWatchState
            appData <- ask
            liftIO $
              FS.withManagerConf conf \watchManager -> do
                events <- atomically SM.empty
                let handler e = unlessM (isLocked (unsafeCoerce watchState) e.eventPath) do
                      tid <- fork do
                        $(logDebug) $ "FileSystem event received; waiting for 100ms: " <> from (show e)
                        tid <- myThreadId
                        threadDelay 100000
                        latest <- atomically do
                          SM.lookup e.eventPath events >>= \case
                            Just (e', tid') | e' == e && tid == tid' -> do
                              SM.delete e.eventPath events
                              pure True
                            _ -> pure False
                        $(logDebug) $ "Event is latest after 100ms: " <> showt latest
                        when latest do
                          handleEvent appData ref e
                      atomically $ SM.insert e.eventPath (e, tid) events
                stop <- FS.watchTree watchManager p (\e -> takeExtension e.eventPath `notElem` [".tmp", ".part"]) handler
                atomically $ modifyTVar' (unsafeCoerce watchState.stoppers) (H.insert ref stop)
                forever $ threadDelay 1000000
    where
      handleException (SomeException e) = do
        let cause = displayException e
        $(logErrorVIO ['cause]) "Failure occured in filesystem watcher thread"
  stopWatching ref = do
    watchState <- getWatchState
    stoppers' <- atomically $ readTVar watchState.stoppers
    case H.lookup ref stoppers' of
      Just stop -> liftIO stop
      Nothing -> pure ()
    $(logInfo) $ "Stopped watching collection " <> showt ref

handleEvent ::
  AppData IO ->
  CollectionRef ->
  FS.Event ->
  IO ()
handleEvent appData ref event = runReaderT' appData $ unlessM isLocked' do
  case event of
    FS.Added p _ FS.IsDirectory -> do
      -- skip added directories to avoid reading half-written files
      -- the files should get their own events when finished writing (after debounced)
      $(logInfo) $ "directory added; skipping " <> showt p
    FS.Added p _ FS.IsFile -> do
      $(logInfo) $ "file added; scanning " <> showt p
      liftIO $ runParIO (scanPath appData ScanAll ref p)
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> showt p
      liftIO $ runParIO (scanPath appData ScanNewOrModified ref p)
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> showt p
      liftIO $ runParIO (scanPath appData ScanNewOrModified ref p)
      pure ()
    FS.WatchedDirectoryRemoved p _ _ -> do
      $(logInfo) $ "watched directory removed " <> showt p
      let uri = fileUri p
      refs <- getKeysByUriPrefix uri
      void $ Repo.delete @SourceEntity refs
    FS.Removed p _ isDir -> do
      let uri = fileUri p
      if isDir == FS.IsDirectory
        then do
          $(logInfo) $ "directory removed " <> showt p
          refs <- getKeysByUriPrefix uri
          void $ Repo.delete @SourceEntity refs
        else do
          $(logInfo) $ "file removed " <> showt p
          refs <- getKeysByUri (V.singleton uri)
          void $ Repo.delete @SourceEntity refs
    FS.Unknown p _ _ s ->
      $(logWarn) $ "unknown file system event on path " <> showt p <> ": " <> T.pack s
  where
    isLocked' = do
      watchState <- getWatchState
      isLocked watchState event.eventPath

isLocked :: (MonadConc m) => CollectionWatchState m -> FilePath -> m Bool
isLocked watchState p = do
  !locks <- atomically $ readTVar watchState.locks
  pure $ isPathLocked p locks
