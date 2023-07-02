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
import Data.ByteString ((!?))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 (ByteString, pack)
import Data.Char
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty qualified as NE
import Data.Monoid
import Data.Pool
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Trie (Trie ())
import Data.Trie qualified as Trie
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Metadata.Aggregate
import Melo.Common.Uri
import Melo.Database.Repo qualified as Repo
import Melo.Database.Repo.IO (DbConnection(..))
import Melo.Format.Error qualified as F
import Melo.Format.Metadata (MetadataFile (..), fileFactoryByExt)
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Types
import Melo.Library.Release.Aggregate
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo
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
import Network.HTTP.Client qualified as Http
import System.FSNotify (ThreadingMode (..))
import System.FSNotify qualified as FS
import System.FilePath
import UnliftIO.Directory qualified as Dir

data ScanType = ScanNewOrModified | ScanAll
  deriving (Eq)

scanPathIO ::
  Pool Connection ->
  Http.Manager ->
  CollectionWatchState ->
  ScanType ->
  CollectionRef ->
  FilePath ->
  ParIO ()
scanPathIO pool manager cws scanType ref p' =
  do
    p <- Dir.canonicalizePath p'
    $(logInfoIO) $ "Scanning " <> showt p
    if scanType == ScanNewOrModified
      then $(logDebugIO) $ "Looking for updated/new files in " <> showt p
      else $(logDebugIO) $ "Looking for files in " <> showt p
    isDir <- Dir.doesDirectoryExist p
    isFile <- Dir.doesFileExist p
    srcs <-
      if isDir
        then do
          $(logDebugIO) $ showt p <> " is directory; recursing..."
          entries <- Dir.listDirectory p
          dirs <- filterM Dir.doesDirectoryExist ((p </>) <$> entries)

          _ <- parMapM (scanPathIO pool manager cws scanType ref) dirs

          files <- filterM Dir.doesFileExist ((p </>) <$> entries)
          let cuefiles = filter ((== ".cue") . takeExtension) files
          case cuefiles of
            [] -> handleScanErrors files $ importTransaction files
            [cuefile] -> do
              $(logDebugIO) $ "Cue file found " <> showt cuefile
              liftIO $
                handleAny (logShow files >=> \_ -> handleScanErrors files $ importTransaction files) $
                  V.length
                    <$> runImport
                      pool
                      manager
                      ( openCueFile cuefile <&> (CueFileImportSource ref <$>) >>= importSources
                      )
            _ -> do
              $(logWarnIO) $ "Multiple cue file found in " <> showt p <> "; skipping..."
              pure 0
        else
          if isFile
            then liftIO $ handleScanErrors [p] $ importTransaction [p]
            else pure 0
    $(logInfoIO) $ showt srcs <> " sources imported from path " <> showt p
    pure ()
  where
    handleScanErrors :: (MonadIO m) => [FilePath] -> IO Int -> m Int
    handleScanErrors ps = liftIO . handleAny (logShow ps >=> \_ -> pure 0)
    importTransaction :: [FilePath] -> IO Int
    importTransaction files = do
      filterImports files >>= \case
        [] -> pure 0
        files -> do
          $(logDebugIO) $ "Importing " <> showt files
          mfs <- catMaybes <$> mapM openMetadataFile'' files
          $(logDebugIO) $ "Opened " <> showt (mfs <&> \mf -> mf.filePath)
          srcs <-
            runImport pool manager $
              importSources $
                V.fromList (FileSource ref <$> mfs)
          pure (V.length srcs)
    runImport pool manager =
      runConfigRepositoryIO (Pooled pool)
        . runSourceRepositoryIO (Pooled pool)
        . runArtistRepositoryIO (Pooled pool)
        . runArtistNameRepositoryIO (Pooled pool)
        . runReleaseArtistNameRepositoryIO (Pooled pool)
        . runReleaseRepositoryIO (Pooled pool)
        . runTrackRepositoryIO (Pooled pool)
        . runTrackArtistNameRepositoryIO (Pooled pool)
        . MB.runMusicBrainzServiceIO manager
        . runTagMappingRepositoryIO (Pooled pool)
        . MB.runCachingMusicBrainzService
        . runTagMappingAggregate
        . runFileSystemWatcherIO pool cws manager
        . runMetadataAggregateIO
        . runArtistAggregateIOT
        . runTrackAggregateIOT
        . runReleaseAggregateIOT
        . runSourceAggregateIOT
    openMetadataFile'' p =
      runConfigRepositoryIO (Pooled pool) $
        runFileSystemWatcherIO pool cws manager $
          runMetadataAggregateIO $
            openMetadataFileByExt p >>= \case
              Right mf -> pure $ Just mf
              Left e@F.UnknownFormat -> do
                let cause = displayException e
                $(logWarnVIO ['cause]) $ "Could not open by extension " <> showt p
                openMetadataFile p >>= \case
                  Left e -> do
                    let cause = displayException e
                    $(logErrorVIO ['cause]) $ "Could not open " <> showt p
                    pure Nothing
                  Right mf -> pure $ Just mf
              Left e -> do
                let cause = displayException e
                $(logErrorVIO ['cause]) $ "Could not open by extension " <> showt p
                pure Nothing
    logShow :: [FilePath] -> SomeException -> IO ()
    logShow filePaths e =
      let !cause = displayException e
       in $(logErrorVIO ['filePaths, 'cause]) "error during scan"
    filterImports :: [FilePath] -> IO [FilePath]
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
    sourceMap :: [FilePath] -> IO (H.HashMap Text SourceEntity)
    sourceMap files = runSourceRepositoryIO (Pooled pool) do
      ss <- V.toList <$> getByUri (V.fromList $ fileUri <$> files)
      pure $ H.fromList $ (\s -> (s.source_uri, s)) <$> ss

data CollectionWatchState = CollectionWatchState
  { stoppers :: STM.TVar (H.HashMap CollectionRef FS.StopListening),
    locks :: STM.TVar FilePathLocks
  }

emptyWatchState :: IO CollectionWatchState
emptyWatchState =
  STM.atomically $
    CollectionWatchState <$> STM.newTVar mempty <*> STM.newTVar mempty

newtype FileSystemWatcherIOT m a = FileSystemWatcherIOT
  { runFileSystemWatcherIOT :: ReaderT (Pool Connection, CollectionWatchState, Http.Manager) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader (Pool Connection, CollectionWatchState, Http.Manager),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadBase b,
      MonadBaseControl b,
      PrimMonad
    )

runFileSystemWatcherIO ::
  Pool Connection -> CollectionWatchState -> Http.Manager -> FileSystemWatcherIOT m a -> m a
runFileSystemWatcherIO pool watchState manager =
  flip runReaderT (pool, watchState, manager) . runFileSystemWatcherIOT

instance
  ( MonadIO m,
    MonadMask m,
    MonadBaseControl IO m,
    Logging m
  ) =>
  FileSystemWatcher (FileSystemWatcherIOT m)
  where
  startWatching ref p = do
    (pool, watchState, manager) <- ask
    $(logInfo) $ "starting to watch path " <> showt p
    let conf = FS.defaultConfig {FS.confThreadingMode = ThreadPerWatch, FS.confOnHandlerException = handleException}
    void $
      liftIO $
        forkIO $
          forever $
            handle handleException $
              FS.withManagerConf conf \watchManager -> do
                events <- STM.atomically SM.empty
                let handler e = unlessM (isLocked watchState e.eventPath) do
                      tid <- forkIO do
                        $(logDebugIO) $ "FileSystem event received; waiting for 100ms: " <> from (show e)
                        tid <- myThreadId
                        threadDelay 100000
                        latest <- STM.atomically do
                          SM.lookup e.eventPath events >>= \case
                            Just (e', tid') | e' == e && tid == tid' -> do
                              SM.delete e.eventPath events
                              pure True
                            _ -> pure False
                        $(logDebugIO) $ "Event is latest after 100ms: " <> showt latest
                        when latest do
                          handleEvent pool manager watchState ref e
                      STM.atomically $ SM.insert e.eventPath (e, tid) events
                stop <- FS.watchTree watchManager p (\e -> takeExtension e.eventPath `notElem` [".tmp", ".part"]) handler
                STM.atomically $ STM.modifyTVar' watchState.stoppers (H.insert ref stop)
                forever $ threadDelay 1000000
    where
      handleException (SomeException e) = do
        let cause = displayException e
        $(logErrorVIO ['cause]) "Failure occured in filesystem watcher thread"
  stopWatching ref = do
    (_pool, watchState, _) <- ask
    stoppers' <- liftIO $ STM.atomically $ STM.readTVar watchState.stoppers
    case H.lookup ref stoppers' of
      Just stop -> liftIO stop
      Nothing -> pure ()
  lockPathsDuring ps m = do
    (_pool, watchState, _sess) <- ask
    bracket
      ( liftIO $ do
          STM.atomically $ STM.modifyTVar' watchState.locks (lockPaths ps)
          $(logInfoIO) $ "Unwatching paths " <> showt (NE.toList ps)
      )
      ( \_ -> liftIO $ forkIO do
          threadDelay 10000
          $(logInfoIO) $ "Re-watching paths " <> showt (NE.toList ps)
          STM.atomically $ STM.modifyTVar' watchState.locks (unlockPaths ps)
      )
      (const m)

data FilePathLocks = FilePathLocks
  { lockCounter :: Trie (Sum Int)
  }
  deriving (Show)
  deriving TextShow via FromStringShow FilePathLocks

instance Semigroup FilePathLocks where
  a <> b = FilePathLocks (a.lockCounter <> b.lockCounter)

instance Monoid FilePathLocks where
  mempty = FilePathLocks mempty

lockPaths :: NonEmpty FilePath -> FilePathLocks -> FilePathLocks
lockPaths newPaths locks = foldl' (\l newPath -> FilePathLocks $ Trie.alterBy lockPath (pack newPath) 1 l.lockCounter) locks newPaths
  where
    lockPath :: ByteString -> Sum Int -> Maybe (Sum Int) -> Maybe (Sum Int)
    lockPath _ c Nothing = Just c
    lockPath _ c (Just c') = Just (c <> c')

unlockPaths :: NonEmpty FilePath -> FilePathLocks -> FilePathLocks
unlockPaths oldPaths locks = foldl' (\l oldPath -> FilePathLocks $ Trie.alterBy unlockPath (pack oldPath) (-1) l.lockCounter) locks oldPaths
  where
    unlockPath :: ByteString -> Sum Int -> Maybe (Sum Int) -> Maybe (Sum Int)
    unlockPath _ _ Nothing = Nothing
    unlockPath _ _ (Just 1) = Nothing
    unlockPath _ c (Just c') = Just (c <> c')

isPathLocked :: FilePath -> FilePathLocks -> Bool
isPathLocked path locks = validMatch $ Trie.matches locks.lockCounter (pack path)
  where
    validMatch :: [(ByteString, Sum Int, ByteString)] -> Bool
    validMatch ((_, _, rem) : _) | BS.null rem = True
    validMatch ((_, _, rem) : _) | rem !? 0 == Just (fromIntegral $ ord '/') = True
    validMatch (_ : xs) = validMatch xs
    validMatch [] = False

handleEvent ::
  Pool Connection ->
  Http.Manager ->
  CollectionWatchState ->
  CollectionRef ->
  FS.Event ->
  IO ()
handleEvent pool manager cws ref event = unlessM (isLocked cws event.eventPath) do
  case event of
    FS.Added p _ FS.IsDirectory -> do
      -- skip added directories to avoid reading half-written files
      -- the files should get their own events when finished writing (after debounced)
      $(logInfo) $ "directory added; skipping " <> showt p
    FS.Added p _ FS.IsFile -> do
      $(logInfo) $ "file added; scanning " <> showt p
      liftIO $ runParIO (scanPathIO pool manager cws ScanAll ref p)
      pure ()
    FS.Modified p _ _ -> do
      $(logInfo) $ "file/directory modified; scanning " <> showt p
      liftIO $ runParIO (scanPathIO pool manager cws ScanNewOrModified ref p)
      pure ()
    FS.ModifiedAttributes p _ _ -> do
      $(logInfo) $ "file/directory attributes modified; scanning " <> showt p
      liftIO $ runParIO (scanPathIO pool manager cws ScanNewOrModified ref p)
      pure ()
    FS.WatchedDirectoryRemoved p _ _ -> do
      $(logInfo) $ "watched directory removed " <> showt p
      let uri = fileUri p
      runSourceRepositoryIO (Pooled pool) do
        refs <- getKeysByUriPrefix uri
        void $ Repo.delete @SourceEntity refs
    FS.Removed p _ isDir -> do
      let uri = fileUri p
      if isDir == FS.IsDirectory
        then do
          $(logInfo) $ "directory removed " <> showt p
          runSourceRepositoryIO (Pooled pool) do
            refs <- getKeysByUriPrefix uri
            void $ Repo.delete @SourceEntity refs
        else do
          $(logInfo) $ "file removed " <> showt p
          runSourceRepositoryIO (Pooled pool) do
            refs <- getKeysByUri (V.singleton uri)
            void $ Repo.delete @SourceEntity refs
    FS.Unknown p _ _ s ->
      $(logWarn) $ "unknown file system event on path " <> showt p <> ": " <> T.pack s

isLocked :: CollectionWatchState -> FilePath -> IO Bool
isLocked watchState p = do
  !locks <- STM.atomically $ STM.readTVar watchState.locks
  pure $ isPathLocked p locks
