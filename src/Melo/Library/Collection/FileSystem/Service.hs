{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Service where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Base
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Maybe
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
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
  { runFileSystemServiceIOT :: m a
  }
  deriving (Functor, Applicative, Monad, MonadBase b, MonadBaseControl b, MonadIO, MonadConc, MonadCatch, MonadMask, MonadThrow)
  deriving (MonadTrans, MonadTransControl) via IdentityT

runFileSystemServiceIO :: FileSystemServiceIOT m a -> m a
runFileSystemServiceIO = runFileSystemServiceIOT

runFileSystemServiceIO' ::
  ( MonadIO m,
    MonadConc m
  ) =>
  Pool Connection ->
  FileSystemServiceIOT (SourceRepositoryIOT (SavepointIOT (MetadataServiceIOT (FileSystemIOT (TransactionIOT m))))) a ->
  m ()
runFileSystemServiceIO' pool m = void $
  runTransaction pool $
    withTransaction $ \conn ->
      runFileSystemIO $ runMetadataServiceIO $ runSavepoint conn $ runSourceRepositoryIO conn $ runFileSystemServiceIO m

forkFileSystemServiceIO ::
  ( MonadIO m,
    MonadConc m
  ) =>
  Pool Connection ->
  FileSystemServiceIOT (SourceRepositoryIOT (SavepointIOT (MetadataServiceIOT (FileSystemIOT (TransactionIOT m))))) a ->
  m ()
forkFileSystemServiceIO pool m = void $ fork $ runFileSystemServiceIO' pool m

instance
  ( MonadIO m,
    MonadCatch m,
    MetadataService m,
    SourceRepository m,
    Savepoint m,
    FileSystem m,
    Logging m
  ) =>
  FileSystemService (FileSystemServiceIOT m)
  where
  scanPath ref p' = FileSystemServiceIOT $
    handle handleScanException $ do
      p <- canonicalizePath p'
      $(logInfo) $ "Importing " <> p
      isDir <- doesDirectoryExist p
      isFile <- doesFileExist p
      srcs <-
        if isDir
          then do
            $(logDebug) $ p <> " is directory; recursing..."
            dirs <- filterM doesDirectoryExist =<< listDirectoryAbs p
            runFileSystemServiceIO $ mapM_ (scanPath ref) dirs
            files <- filterM doesFileExist =<< listDirectoryAbs p
            if any ((== ".cue") . takeExtension) files
              then do
                -- TODO load file(s) referenced in cuefile
                $(logWarn) $ "Cue file found in " <> show p <> "; skipping..."
                pure mempty
              else withSavepoint do
                $(logDebug) $ "Importing " <> show files
                mfs <- catMaybes <$> mapM openMetadataFile'' files
                $(logDebug) $ "Opened " <> show mfs
                importSources (FileSource ref <$> mfs)
          else
            if isFile
              then withSavepoint do
                mf <- openMetadataFile'' p
                importSources (FileSource ref <$> maybeToList mf)
              else pure mempty
      $(logInfo) $ "Import finished: " <> show srcs
      pure srcs
    where
      openMetadataFile'' :: FilePath -> m (Maybe F.MetadataFile)
      openMetadataFile'' p =
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
      handleScanException :: SomeException -> m [Source]
      handleScanException e = do
        $(logError) $ "error during scan: " <> show e
        pure []

listDirectoryAbs :: FileSystem m => FilePath -> m [FilePath]
listDirectoryAbs p = do
  es <- listDirectory p
  pure $ (p </>) <$> sortNaturalBy id es
