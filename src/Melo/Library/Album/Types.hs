{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Album.Types where

import Control.Lens hiding (from)
import Data.Foldable
import Data.Hashable
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Text (Text)
import Data.Time
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import Melo.Library.Artist.Name.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Rel8
import Witch

data AlbumTable f = AlbumTable
  { id :: Column f AlbumRef,
    title :: Column f Text,
    comment :: Column f (Maybe Text),
    year_released :: Column f (Maybe Text),
    length :: Column f (Maybe CalendarDiffTime),
    musicbrainz_id :: Column f (Maybe Text),
    original_year_released :: Column f (Maybe Text),
    musicbrainz_group_id :: Column f (Maybe Text),
    catalogue_number :: Column f (Maybe Text)
  }
  deriving (Generic, Rel8able)

type AlbumEntity = AlbumTable Result

deriving instance Show AlbumEntity

deriving instance Eq AlbumEntity

instance Entity AlbumEntity where
  type NewEntity AlbumEntity = NewAlbum
  type PrimaryKey AlbumEntity = AlbumRef
  primaryKey e = e.id

newtype AlbumRef = AlbumRef UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)

instance GQLType AlbumRef where
  type KIND AlbumRef = SCALAR

instance EncodeScalar AlbumRef where
  encodeScalar (AlbumRef uuid) = M.String $ toText uuid

instance DecodeScalar AlbumRef where
  decodeScalar (M.String s) = case fromText s of
    Nothing -> Left "AlbumRef must be UUID"
    Just uuid -> Right $ AlbumRef uuid
  decodeScalar _ = Left "AlbumRef must be a String"

instance From AlbumRef UUID where
  from (AlbumRef uuid) = uuid

data Album = Album
  { ref :: AlbumRef,
    title :: Text,
    yearReleased :: Maybe Text,
    originalYearReleased :: Maybe Text,
    catalogueNumber :: Maybe Text,
    artists :: [ArtistNameEntity]
  }
  deriving (Show, Eq, Generic)

mkAlbum :: Foldable f => f ArtistNameEntity -> AlbumEntity -> Album
mkAlbum albumArtists e =
  Album
    { ref = e.id,
      title = e.title,
      yearReleased = e.year_released,
      originalYearReleased = e.original_year_released,
      catalogueNumber = e.catalogue_number,
      artists = toList albumArtists
    }

data NewAlbum = NewAlbum
  { title :: Text,
    comment :: Maybe Text,
    yearReleased :: Maybe Text,
    musicbrainzId :: Maybe MB.MusicBrainzId,
    originalYearReleased :: Maybe Text,
    musicbrainzGroupId :: Maybe MB.MusicBrainzId,
    catalogueNumber :: Maybe Text
  }
  deriving (Generic, Eq, Ord, Show)

fromMusicBrainz :: Maybe MB.ReleaseGroup -> Maybe MB.Release -> Maybe NewAlbum
fromMusicBrainz Nothing Nothing = Nothing
fromMusicBrainz (Just releaseGroup) (Just release) =
  Just
    NewAlbum
      { title = releaseGroup.title,
        yearReleased = release.date,
        originalYearReleased = releaseGroup.firstReleaseDate,
        musicbrainzId = Just release.id,
        musicbrainzGroupId = Just releaseGroup.id,
        catalogueNumber = release ^? #labelInfo . _Just . _head . #catalogNumber . _Just,
        comment = Nothing
      }
fromMusicBrainz (Just releaseGroup) Nothing =
  Just
    NewAlbum
      { title = releaseGroup.title,
        yearReleased = Nothing,
        originalYearReleased = releaseGroup.firstReleaseDate,
        musicbrainzId = Nothing,
        musicbrainzGroupId = Just releaseGroup.id,
        catalogueNumber = Nothing,
        comment = Nothing
      }
fromMusicBrainz Nothing (Just release) =
  Just
    NewAlbum
      { title = release.title,
        yearReleased = release.date,
        originalYearReleased = Nothing,
        musicbrainzId = Just release.id,
        musicbrainzGroupId = Nothing,
        catalogueNumber = release ^? #labelInfo . _Just . _head . #catalogNumber . _Just,
        comment = Nothing
      }

instance From NewAlbum (AlbumTable Expr) where
  from a =
    AlbumTable
      { id = nullaryFunction "uuid_generate_v4",
        title = lit a.title,
        comment = lit Nothing,
        year_released = lit a.yearReleased,
        original_year_released = lit a.originalYearReleased,
        length = lit Nothing,
        musicbrainz_id = lit $ MB.mbid <$> a.musicbrainzId,
        musicbrainz_group_id = lit $ MB.mbid <$> a.musicbrainzGroupId,
        catalogue_number = lit a.catalogueNumber
      }

fromNewAlbum :: NewAlbum -> UUID -> AlbumEntity
fromNewAlbum a ref =
  AlbumTable
    { id = AlbumRef ref,
      title = a.title,
      comment = Nothing,
      year_released = a.yearReleased,
      original_year_released = a.originalYearReleased,
      length = Nothing,
      musicbrainz_id = MB.mbid <$> a.musicbrainzId,
      musicbrainz_group_id = MB.mbid <$> a.musicbrainzGroupId,
      catalogue_number = a.catalogueNumber
    }
