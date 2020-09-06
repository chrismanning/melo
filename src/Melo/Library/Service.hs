{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Service where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.TH
import qualified Control.Exception.Safe as E
import Control.Monad
import Data.Functor
import Data.Maybe (catMaybes, maybeToList)
import Melo.Common.Effect
import Melo.Common.FileSystem
import Melo.Common.Logging
import qualified Melo.Format.Metadata as F
import qualified Melo.Library.Source.Repo as SourceRepo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import System.FilePath

data LibraryService :: Effect where
  ImportPath :: FilePath -> LibraryService m [Source]

makeSmartConstructors ''LibraryService

newtype LibraryServiceIOC m a = LibraryServiceIOC
  { runLibraryServiceIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has (Lift IO) sig m,
    Has FileSystem sig m,
    Has SourceRepo.SourceRepository sig m,
    Has Logging sig m
  ) =>
  Algebra (LibraryService :+: sig) (LibraryServiceIOC m)
  where
  alg _ (L (ImportPath p')) ctx = do
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
          mapM_ importPath dirs
          files <- filterM doesFileExist =<< listDirectoryAbs p
          if any ((== ".cue") . takeExtension) files
            then do
              -- TODO load file(s) referenced in cuefile
              $(logWarn) $ "Cue file found in " <> show p <> "; skipping..."
              pure mempty
            else do
              $(logDebug) $ "Importing " <> show files
              mfs <- catMaybes <$> mapM openMetadataFile files
              $(logDebug) $ "Opened " <> show mfs
              importSources (FileSource <$> mfs)
        else
          if isFile
            then do
              mf <- openMetadataFile p
              importSources $ FileSource <$> maybeToList mf
            else pure mempty
    $(logInfo) $ "Import finished: " <> show srcs
    pure (ctx $> srcs)
  alg hdl (R other) ctx = LibraryServiceIOC (alg (runLibraryServiceIOC . hdl) other ctx)

listDirectoryAbs :: Has FileSystem sig m => FilePath -> m [FilePath]
listDirectoryAbs p = do
  es <- listDirectory p
  pure $ (p </>) <$> es

openMetadataFile ::
  ( Has (Lift IO) sig m,
    Has Logging sig m
  ) =>
  FilePath ->
  m (Maybe F.MetadataFile)
openMetadataFile p = do
  $(logDebug) $ "Opening file " <> p
  sendIO (E.tryAny (F.openMetadataFile p)) >>= \case
    Left e -> do
      $(logWarn) $ show e <> ": " <> p
      pure Nothing
    Right mf -> pure (Just mf)

runLibraryServiceIO :: LibraryServiceIOC m a -> m a
runLibraryServiceIO = runLibraryServiceIOC
