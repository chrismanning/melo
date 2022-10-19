{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Service
  ( FileSystemService (..),
    FileSystemServiceIOT (..),
    runFileSystemServiceIO,
    runFileSystemServiceIO',
    forkFileSystemServiceIO,
  )
where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Functor ((<&>))
import Data.Map.Strict qualified as H
import Data.Maybe
import Data.Pool
import Data.Text qualified as T
import Data.Time.LocalTime
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Exts (groupWith)
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
    Source (..),
    SourceEntity,
    SourceTable (..),
  )
import System.Directory (getModificationTime)
import System.FilePath

class Monad m => FileSystemService m where
  scanPath :: CollectionRef -> FilePath -> m (Vector Source)
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

forkFileSystemServiceIO ::
  ( MonadIO m,
    MonadConc m
  ) =>
  Pool Connection ->
  ImportT m a ->
  m ()
forkFileSystemServiceIO pool m = void $ fork $ runFileSystemServiceIO' pool m

instance
  ( MonadIO m,
    MonadConc m,
    FileSystem m,
    Logging m
  ) =>
  FileSystemService (FileSystemServiceIOT m)
  where
  scanPath = scanPathImpl False
  scanPathUpdates ref p = void $ scanPathImpl True ref p

scanPathImpl ::
  forall m.
  ( FileSystem m,
    Logging m,
    MonadConc m,
    MonadIO m
  ) =>
  Bool ->
  CollectionRef ->
  FilePath ->
  FileSystemServiceIOT m (Vector Source)
scanPathImpl onlyNewer ref p' =
  handle handleScanException do
    pool <- ask
    p <- canonicalizePath p'
    $(logInfo) $ "Scanning " <> show p
    if onlyNewer
      then $(logDebug) $ "Looking for updated/new files in " <> show p
      else $(logDebug) $ "Looking for new files in " <> show p
    isDir <- doesDirectoryExist p
    isFile <- doesFileExist p
    srcs <-
      if isDir
        then do
          $(logDebug) $ p <> " is directory; recursing..."
          entries <- listDirectory p
          dirs <- filterM (doesDirectoryExist . (p </>)) entries
          let initialChunks = groupWith head dirs
          fork $
            forM_ initialChunks (mapM_ (fork . void . scanPathImpl onlyNewer ref . (p </>)))
          files <- filterM doesFileExist ((p </>) <$> entries)
          let cuefiles = filter ((== ".cue") . takeExtension) files
          case cuefiles of
            [] -> lift $ withTransaction pool runSourceRepositoryIO (importTransaction files)
            [cuefile] -> do
              $(logInfo) $ "Cue file found " <> show cuefile
              srcs <- lift $ withTransaction pool runSourceRepositoryIO (openCueFile cuefile <&> (CueFileImportSource ref <$>) >>= importSources)
              $(logDebug) $ "Imported sources from cue file: " <> show srcs
              pure srcs
            _ -> do
              $(logWarn) $ "Multiple cue file found in " <> show p <> "; skipping..."
              pure V.empty
        else
          if isFile
            then
              lift $
                ifM
                  (runSourceRepositoryPooledIO pool $ shouldImport [p])
                  (withTransaction pool runSourceRepositoryIO (importTransaction [p]))
                  (pure V.empty)
            else pure V.empty
    $(logInfo) $ show (length srcs) <> " sources imported"
    pure srcs
  where
    importTransaction :: [FilePath] -> SourceRepositoryIOT m (Vector Source)
    importTransaction files = do
      ifM
        (shouldImport files)
        ( do
            $(logDebug) $ "Importing " <> show files
            mfs <- catMaybes <$> mapM openMetadataFile'' files
            $(logDebug) $ "Opened " <> show (mfs <&> \mf -> mf.filePath)
            importSources $ V.fromList (FileSource ref <$> mfs)
        )
        (pure V.empty)
    openMetadataFile'' p =
      runMetadataServiceIO $
        openMetadataFileByExt p >>= \case
          Right mf -> pure $ Just mf
          Left e@F.UnknownFormat -> do
            $(logWarn) $ "Could not open by extension " <> p <> ": " <> show e
            openMetadataFile p >>= \case
              Left e -> do
                $(logError) $ "Could not open " <> p <> ": " <> show e
                pure Nothing
              Right mf -> pure $ Just mf
          Left e -> do
            $(logError) $ "Could not open by extension " <> p <> ": " <> show e
            pure Nothing
    handleScanException :: SomeException -> FileSystemServiceIOT m (Vector Source)
    handleScanException e = do
      $(logError) $ "error during scan: " <> show e
      pure V.empty
    shouldImport :: [FilePath] -> SourceRepositoryIOT m Bool
    shouldImport _ | onlyNewer == False = pure True
    shouldImport files = do
      tz <- liftIO getCurrentTimeZone
      ss <- sourceMap files
      updated <- liftIO $ forM files $ \p ->
        case H.lookup (T.pack $ show $ fileUri p) ss of
          Just s -> do
            mtime <- utcToLocalTime tz <$> getModificationTime p
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
    sourceMap :: [FilePath] -> SourceRepositoryIOT m (H.Map T.Text SourceEntity)
    sourceMap files = do
      ss <- V.toList <$> getByUri (V.fromList $ fileUri <$> files)
      pure $ H.fromList $ (\s -> (s.source_uri, s)) <$> ss
