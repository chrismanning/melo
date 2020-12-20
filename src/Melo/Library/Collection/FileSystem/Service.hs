{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.Service where

import Control.Algebra
import Control.Effect.Catch
import Control.Effect.Exception
import Control.Effect.TH
import Control.Monad
import Data.Functor
import Data.Maybe
import Melo.Common.Effect
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Database.Transaction
import qualified Melo.Format as F
import qualified Melo.Format.Error as F
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import System.FilePath

data FileSystemService :: Effect where
  ScanPath :: CollectionRef -> FilePath -> FileSystemService m [Source]

makeSmartConstructors ''FileSystemService

newtype FileSystemServiceIOC m a = FileSystemServiceIOC
  { runFileSystemServiceIOC :: m a
  }
  deriving (Functor, Applicative, Monad)

runFileSystemServiceIO :: FileSystemServiceIOC m a -> m a
runFileSystemServiceIO = runFileSystemServiceIOC

instance
  ( Has (Lift IO) sig m,
    Has (Catch F.MetadataException) sig m,
    Has MetadataService sig m,
    Has SourceRepository sig m,
    Has Savepoint sig m,
    Has FileSystem sig m,
    Has Logging sig m
  ) =>
  Algebra (FileSystemService :+: sig) (FileSystemServiceIOC m)
  where
  alg hdl sig ctx = case sig of
    L (ScanPath ref p') -> fmap (ctx $>) $ handle handleScanException $ withSavepoint $ do
      -- TODO IO error handling
      p <- canonicalizePath p'
      $(logInfo) $ "Importing " <> p
      isDir <- doesDirectoryExist p
      isFile <- doesFileExist p
      srcs <-
        if isDir
          then do
            $(logDebug) $ p <> " is directory; recursing..."
            dirs <- filterM doesDirectoryExist =<< listDirectoryAbs p
            mapM_ (scanPath ref) dirs
            files <- filterM doesFileExist =<< listDirectoryAbs p
            if any ((== ".cue") . takeExtension) files
              then do
                -- TODO load file(s) referenced in cuefile
                $(logWarn) $ "Cue file found in " <> show p <> "; skipping..."
                pure mempty
              else do
                $(logDebug) $ "Importing " <> show files
                mfs <- catMaybes <$> mapM openMetadataFile'' files
                $(logDebug) $ "Opened " <> show mfs
                importSources (FileSource ref <$> mfs)
          else
            if isFile
              then do
                mf <- openMetadataFile'' p
                importSources (FileSource ref <$> maybeToList mf)
              else pure mempty
      $(logInfo) $ "Import finished: " <> show srcs
      pure srcs
    R other -> FileSystemServiceIOC (alg (runFileSystemServiceIOC . hdl) other ctx)
    where
      openMetadataFile'' :: FilePath -> FileSystemServiceIOC m (Maybe F.MetadataFile)
      openMetadataFile'' p =
        openMetadataFileByExt' p >>= \case
          Right mf -> pure $ Just mf
          Left e -> do
            $(logWarn) $ "Could not open by extension " <> p <> ": " <> show e
            openMetadataFile' p >>= \case
              Left e -> do
                $(logError) $ "Could not open " <> p <> ": " <> show e
                pure Nothing
              Right mf -> pure $ Just mf
      handleScanException :: SomeException -> FileSystemServiceIOC m [Source]
      handleScanException e = do
        $(logError) $ "error during scan: " <> show e
        pure []

listDirectoryAbs :: Has FileSystem sig m => FilePath -> m [FilePath]
listDirectoryAbs p = do
  es <- listDirectory p
  pure $ (p </>) <$> es
