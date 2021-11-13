{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Service where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import qualified Control.Monad.Parallel as Par
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Pool
import Hasql.Connection
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.NaturalSort
import Melo.Database.Transaction
import qualified Melo.Format as F
import qualified Melo.Format.Error as F
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
  ( NewImportSource (..),
    Source (..),
  )
import System.FilePath

class Monad m => FileSystemService m where
  scanPath :: CollectionRef -> FilePath -> m [Source]

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    FileSystemService m
  ) =>
  FileSystemService (t m)
  where
  scanPath ref p = lift (scanPath ref p)

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
      MonadParallel,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadReader (Pool Connection)
    )

runFileSystemServiceIO :: (MonadIO m) => Pool Connection -> FileSystemServiceIOT (FileSystemIOT m) a -> m a
runFileSystemServiceIO pool = runFileSystemIO . flip runReaderT pool . runFileSystemServiceIOT

type ImportT m = FileSystemServiceIOT (FileSystemIOT m)

runFileSystemServiceIO' ::
  (MonadIO m, MonadCatch m, MonadParallel m) =>
  Pool Connection ->
  ImportT m a ->
  m ()
runFileSystemServiceIO' pool m =
  void $ runFileSystemServiceIO pool m

forkFileSystemServiceIO ::
  ( MonadIO m,
    MonadConc m,
    MonadParallel m
  ) =>
  Pool Connection ->
  ImportT m a ->
  m ()
forkFileSystemServiceIO pool m = void $ fork $ runFileSystemServiceIO' pool m

instance
  ( MonadIO m,
    MonadCatch m,
    MonadMask m,
    MonadParallel m,
    FileSystem m,
    Logging m
  ) =>
  FileSystemService (FileSystemServiceIOT m)
  where
  scanPath ref p' =
    handle handleScanException do
      pool <- ask
      p <- canonicalizePath p'
      $(logInfo) $ "Importing " <> p
      isDir <- doesDirectoryExist p
      isFile <- doesFileExist p
      srcs <-
        if isDir
          then do
            $(logDebug) $ p <> " is directory; recursing..."
            dirs <- filterM doesDirectoryExist =<< listDirectoryAbs p
            Par.mapM_ (scanPath ref) dirs
            files <- filterM doesFileExist =<< listDirectoryAbs p
            if any ((== ".cue") . takeExtension) files
              then do
                -- TODO load file(s) referenced in cuefile
                $(logWarn) $ "Cue file found in " <> show p <> "; skipping..."
                pure mempty
              else lift $ withTransaction pool runSourceRepositoryIO (importTransaction files)
          else
            if isFile
              then lift $ withTransaction pool runSourceRepositoryIO (importTransaction [p])
              else pure mempty
      $(logInfo) $ "Import finished: " <> show srcs
      pure srcs
    where
      importTransaction :: [FilePath] -> SourceRepositoryIOT m [Source]
      importTransaction files = do
        $(logDebug) $ "Importing " <> show files
        mfs <- catMaybes <$> mapM openMetadataFile'' files
        $(logDebug) $ "Opened " <> show mfs
        importSources (FileSource ref <$> mfs)
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
      handleScanException :: SomeException -> FileSystemServiceIOT m [Source]
      handleScanException e = do
        $(logError) $ "error during scan: " <> show e
        pure []

listDirectoryAbs :: FileSystem m => FilePath -> m [FilePath]
listDirectoryAbs p = do
  es <- listDirectory p
  pure $ (p </>) <$> sortNaturalBy (\x -> x) es
