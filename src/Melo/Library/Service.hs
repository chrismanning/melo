{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Service where

import Control.Algebra
import Control.Applicative
import Control.Bool
import Control.Effect.Error
import Control.Effect.Lift
import qualified Control.Exception.Safe as E
import Control.Lens hiding (lens)
import Control.Monad
import Data.Attoparsec.Text
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Maybe (catMaybes, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Format.Ape
import Melo.Format.ID3.ID3v1
import Melo.Format.ID3.ID3v2
import Melo.Format.Info
import qualified Melo.Format.Mapping as M
import qualified Melo.Format.Metadata as F
import Melo.Format.Vorbis
import Melo.Library.Album.Repo
--import Melo.Library.Album.Service
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Service
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Genre.Repo
--import Melo.Library.Genre.Service
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import Melo.Library.Track.Repo
import Melo.Library.Track.Service
import Network.URI
import System.Directory
import System.FilePath

data LibraryService :: Effect where
  ImportPath :: FilePath -> LibraryService m ImportStats

importPath :: Has LibraryService sig m => FilePath -> m ImportStats
importPath p = send (ImportPath p)

newtype LibraryServiceIOC m a = LibraryServiceIOC
  { runLibraryServiceIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has (Lift IO) sig m,
    Has SourceService sig m,
    Has ArtistService sig m,
    Has Logging sig m
  ) =>
  Algebra (LibraryService :+: sig) (LibraryServiceIOC m)
  where
  alg _ (L (ImportPath p')) ctx = do
    -- TODO IO error handling
    p <- sendIO $ canonicalizePath p'
    $(logInfo) $ "Importing " <> p
    isDir <- sendIO $ doesDirectoryExist p
    isFile <- sendIO $ doesFileExist p
    stats <-
      if isDir
        then do
          $(logDebug) $ p <> " is directory; recursing..."
          dirs <- sendIO $ filterM doesDirectoryExist =<< listDirectoryAbs p
          mapM_ importPath dirs
          files <- sendIO $ filterM doesFileExist =<< listDirectoryAbs p
          if any ((== ".cue") . takeExtension) files
            then do
              -- TODO load file(s) referenced in cuefile
              pure mempty
            else do
              $(logDebug) $ "Importing " <> show files
              mfs <- catMaybes <$> mapM openMetadataFile files
              $(logDebug) $ "Opened " <> show mfs
              srcs <- importSources (FileSource <$> mfs)
              artists <- importArtists srcs
              let albums = [] --importAlbums metadataSources
              let tracks = [] --importTracks metadataSources
              pure
                ImportStats
                  { sourcesImported = length' srcs,
                    tracksImported = length' tracks,
                    albumsImported = length' albums,
                    artistsImported = length' artists,
                    genresImported = 0
                  }
        else
          if isFile
            then do
              mf <- openMetadataFile p
              srcs <- importSources $ FileSource <$> maybeToList mf
              pure $
                mempty
                  { sourcesImported = length' srcs
                  }
            else pure mempty
    $(logInfo) $ "Import finished: " <> show stats
    pure (ctx $> stats)
  alg hdl (R other) ctx = LibraryServiceIOC (alg (runLibraryServiceIOC . hdl) other ctx)

listDirectoryAbs :: FilePath -> IO [FilePath]
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
