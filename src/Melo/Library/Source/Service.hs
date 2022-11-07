{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Service where

import Control.Applicative hiding (empty)
import Control.Foldl (PrimMonad)
import Control.Lens hiding (from, lens)
import Data.Char
import Data.Foldable
import qualified Data.Text as T
import Data.Vector (Vector, empty, singleton)
import Data.Vector qualified as V
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Common.Vector
import Melo.Library.Album.ArtistName.Repo
import Melo.Library.Album.Repo
import Melo.Library.Album.Service
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Name.Repo
import qualified Melo.Database.Repo as Repo
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz qualified as MB
import System.FilePath as P
import Witch

getAllSources :: SourceRepository m => m (Vector Source)
getAllSources = rights <$> fmap tryFrom <$> Repo.getAll

getSource :: SourceRepository m => SourceRef -> m (Maybe Source)
getSource key = do
  srcs <- Repo.getByKey (singleton key)
  pure $ firstOf traverse $ rights $ tryFrom <$> srcs

importSources ::
  ( SourceRepository m,
    MB.MusicBrainzService m,
    AlbumRepository m,
    ArtistRepository m,
    ArtistNameRepository m,
    AlbumArtistNameRepository m,
    TrackRepository m,
    TrackArtistNameRepository m,
    PrimMonad m,
    Logging m
  ) =>
  Vector NewImportSource ->
  m (Vector Source)
importSources ss | null ss = pure empty
importSources ss = do
  $(logDebug) $ "Importing " <> show (V.length ss) <> " sources"
  let metadataSources = rights $ fmap tryFrom ss
  $(logDebug) $ "Importing " <> show (V.length metadataSources) <> " metadata sources"
  srcs <- rights . fmap tryFrom <$> Repo.insert @SourceEntity (fmap (from @MetadataImportSource) metadataSources)
  _albums <- importAlbums srcs
  pure srcs

getSourcesByUriPrefix ::
  SourceRepository m =>
  URI ->
  m (Vector Source)
getSourcesByUriPrefix prefix = do
  srcs <- getByUriPrefix prefix
  pure (rights $ fmap tryFrom srcs)

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

getSourceFilePath :: (SourceRepository m) => SourceRef -> m (Maybe FilePath)
getSourceFilePath key = do
  s <- Repo.getSingle key
  pure (s >>= parseURI . T.unpack . (.source_uri) >>= uriToFilePath)

findCoverImage :: FileSystem m => FilePath -> m (Maybe FilePath)
findCoverImage p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- listDirectory p
      pure $
        find (\e -> P.takeBaseName e =~= "cover" && isImage e) entries
          <|> find (\e -> P.takeBaseName e =~= "front" && isImage e) entries
          <|> find (\e -> P.takeBaseName e =~= "folder" && isImage e) entries
    else pure Nothing
  where
    a =~= b = fmap toLower a == fmap toLower b
    isImage :: FilePath -> Bool
    isImage p =
      let ext = toLower <$> takeExtension p
       in ext == ".jpeg"
            || ext == ".jpg"
            || ext == ".png"
