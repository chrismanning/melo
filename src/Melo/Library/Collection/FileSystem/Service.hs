{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Service
  ( FileSystemService (..),
    FileSystemServiceIOT (..),
    runFileSystemServiceIO,
    runFileSystemServiceIO',
    scanPathIO,
    ScanType (..),
  )
where

import Control.Concurrent.Classy (MonadConc)
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Extra
import Control.Monad.Par.Combinator
import Control.Monad.Par.IO
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Functor ((<&>))
import Data.Map.Strict qualified as H
import Data.Maybe
import Data.Pool
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Transaction
import Melo.Format.Error qualified as F
import Melo.Format.Metadata (MetadataFile (..), fileFactoryByExt)
import Melo.Library.Collection.Types
import Melo.Library.Source.Cue
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
  ( NewImportSource (..),
    SourceEntity,
    SourceTable (..),
  )
import System.FilePath
import UnliftIO.Directory qualified as Dir

class Monad m => FileSystemService m where
  scanPath :: CollectionRef -> FilePath -> m ()
  scanPathUpdates :: CollectionRef -> FilePath -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    FileSystemService m
  ) =>
  FileSystemService (t m)
  where
  scanPath ref p = lift (scanPath ref p)
  scanPathUpdates ref p = lift (scanPathUpdates ref p)

newtype FileSystemServiceIOT m a = FileSystemServiceIOT
  { runFileSystemServiceIOT :: ReaderT (Pool Connection) m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadBase b,
      MonadBaseControl b,
      MonadIO,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadReader (Pool Connection)
    )

runFileSystemServiceIO :: (MonadIO m) => Pool Connection -> FileSystemServiceIOT (FileSystemIOT m) a -> m a
runFileSystemServiceIO pool = runFileSystemIO . flip runReaderT pool . runFileSystemServiceIOT

type ImportT m = FileSystemServiceIOT (FileSystemIOT m)

runFileSystemServiceIO' ::
  MonadIO m =>
  Pool Connection ->
  ImportT m a ->
  m ()
runFileSystemServiceIO' pool m =
  void $ runFileSystemServiceIO pool m

instance MonadIO m =>
  FileSystemService (FileSystemServiceIOT m)
  where
  scanPath ref p = ask >>= \pool -> liftIO $ runParIO $ scanPathIO pool ScanAll ref p
  scanPathUpdates ref p = ask >>= \pool -> liftIO $ runParIO $ scanPathIO pool ScanNewOrModified ref p

data ScanType = ScanNewOrModified | ScanAll
  deriving (Eq)

scanPathIO ::
  Pool Connection ->
  ScanType ->
  CollectionRef ->
  FilePath ->
  ParIO ()
scanPathIO pool scanType ref p' =
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

          _ <- parMapM (scanPathIO pool scanType ref) dirs

          files <- filterM Dir.doesFileExist ((p </>) <$> entries)
          let cuefiles = filter ((== ".cue") . takeExtension) files
          case cuefiles of
            [] -> handleScanErrors $ importTransaction files
            [cuefile] -> do
              $(logDebugIO) $ "Cue file found " <> show cuefile
              srcs <- handleScanErrors $ withTransaction pool runSourceRepositoryIO (openCueFile cuefile <&> (CueFileImportSource ref <$>) >>= importSources')
              pure srcs
            _ -> do
              $(logWarnIO) $ "Multiple cue file found in " <> show p <> "; skipping..."
              pure 0
        else
          if isFile
            then
              liftIO $ ifM
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
            runSourceRepositoryPooledIO pool $
              importSources' $ V.fromList (FileSource ref <$> mfs)
        )
        (pure 0)
    openMetadataFile'' p =
      runMetadataServiceIO $
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
        case H.lookup (T.pack $ show $ fileUri p) ss of
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
    sourceMap :: [FilePath] -> IO (H.Map T.Text SourceEntity)
    sourceMap files = runSourceRepositoryPooledIO pool do
      ss <- V.toList <$> getByUri (V.fromList $ fileUri <$> files)
      pure $ H.fromList $ (\s -> (s.source_uri, s)) <$> ss
