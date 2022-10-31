{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Album.Types where

import Data.Char
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Hashable
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import Melo.Library.Artist.Name.Types
import qualified Melo.Lookup.MusicBrainz as MB
import Rel8
import Witch

data AlbumTable f = AlbumTable
  { id :: Column f AlbumRef,
    title :: Column f Text,
    comment :: Column f (Maybe Text),
    year_released :: Column f (Maybe Text),
    length :: Column f (Maybe CalendarDiffTime),
    musicbrainz_id :: Column f (Maybe Text)
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
  { dbId :: AlbumRef,
    title :: Text,
    artists :: [ArtistNameEntity]
  }
  deriving (Generic)

mkAlbum :: [ArtistNameEntity] -> AlbumEntity -> Album
mkAlbum albumArtists e =
  Album {
    dbId = e.id,
    title = e.title,
    artists = albumArtists
  }

data NewAlbum = NewAlbum
  { title :: Text,
    comment :: Maybe Text,
    yearReleased :: Maybe Text,
    musicbrainzId :: Maybe MB.MusicBrainzId
  }
  deriving (Generic, Eq, Ord, Show)

instance From MB.Release NewAlbum where
  from mbRelease =
    NewAlbum
      { title = mbRelease.title,
        yearReleased = truncateMusicBrainzDate <$> mbRelease.date,
        musicbrainzId = Just mbRelease.id,
        comment = Nothing
      }

instance From MB.ReleaseGroup NewAlbum where
  from mbRelease =
    NewAlbum
      { title = mbRelease.title,
        yearReleased = truncateMusicBrainzDate <$> mbRelease.firstReleaseDate,
        musicbrainzId = Just mbRelease.id,
        comment = Nothing
      }

truncateMusicBrainzDate :: Text -> Text
truncateMusicBrainzDate = T.takeWhile isDigit

instance From NewAlbum (AlbumTable Expr) where
  from a =
    AlbumTable
      { id = nullaryFunction "uuid_generate_v4",
        title = lit a.title,
        comment = lit Nothing,
        year_released = lit a.yearReleased,
        length = lit Nothing,
        musicbrainz_id = lit $ MB.mbid <$> a.musicbrainzId
      }
