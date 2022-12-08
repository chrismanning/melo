{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Scan
  ( scanPathIO,
    ScanType (..),
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Reader
import Data.Functor ((<&>))
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Pool
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Format.Error qualified as F
import Melo.Format.Metadata (MetadataFile (..), fileFactoryByExt)
import Melo.Library.Album.Aggregate
import Melo.Library.Album.ArtistName.Repo
import Melo.Library.Album.Repo
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
import System.FilePath
import UnliftIO.Directory qualified as Dir

data ScanType = ScanNewOrModified | ScanAll
  deriving (Eq)

scanPathIO ::
  Pool Connection ->
  Wreq.Session ->
  ScanType ->
  CollectionRef ->
  FilePath ->
  ParIO ()
scanPathIO pool sess scanType ref p' =
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

          _ <- parMapM (scanPathIO pool sess scanType ref) dirs

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
                  (importTransaction [p])
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
        . runAlbumArtistNameRepositoryPooledIO pool
        . runAlbumRepositoryPooledIO pool
        . runTrackRepositoryPooledIO pool
        . runTrackArtistNameRepositoryPooledIO pool
        . MB.runMusicBrainzServiceUnlimitedIO sess
        . runTagMappingRepositoryPooledIO pool
        . MB.runCachingMusicBrainzService
        . runTagMappingAggregate
        . runMetadataAggregateIO
        . runArtistAggregateIOT
        . runTrackAggregateIOT
        . runAlbumAggregateIOT
        . runSourceAggregateIOT
    openMetadataFile'' p =
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
