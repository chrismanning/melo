{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Artist.Types where

import Control.Applicative
import Control.Lens
import Country
import Data.Hashable
import Data.Maybe
import Data.Morpheus.Types as M
import Data.Morpheus.Kind
import Data.Text (Text)
import Data.UUID
import GHC.Generics hiding (from, to)
import Melo.Database.Repo
import qualified Melo.Lookup.MusicBrainz as MB
import Rel8
import Witch

data ArtistTable f = ArtistTable
  { id :: Column f ArtistRef,
    name :: Column f Text,
    disambiguation :: Column f (Maybe Text),
    short_bio :: Column f (Maybe Text),
    bio :: Column f (Maybe Text),
    country :: Column f (Maybe Text),
    musicbrainz_id :: Column f (Maybe Text)
  }
  deriving (Generic, Rel8able)

type ArtistEntity = ArtistTable Result

deriving instance Show ArtistEntity
deriving instance Eq ArtistEntity
deriving instance Ord ArtistEntity

instance Entity ArtistEntity where
  type NewEntity ArtistEntity = NewArtist
  type PrimaryKey ArtistEntity = ArtistRef
  primaryKey e = e.id

newtype ArtistRef = ArtistRef UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)

instance GQLType ArtistRef where
  type KIND ArtistRef = SCALAR

instance EncodeScalar ArtistRef where
  encodeScalar (ArtistRef uuid) = M.String $ toText uuid

instance DecodeScalar ArtistRef where
  decodeScalar (M.String s) = case fromText s of
    Nothing -> Left "ArtistRef must be UUID"
    Just uuid -> Right $ ArtistRef uuid
  decodeScalar _ = Left "ArtistRef must be a String"

instance From ArtistRef UUID where
  from (ArtistRef uuid) = uuid

data Artist = Artist
  { key :: ArtistRef,
    artistIds :: [ArtistId],
    name :: Text,
    aliases :: [Text],
    disambiguation :: Maybe Text,
    country :: Maybe Country,
    biography :: Maybe Text,
    shortBiography :: Maybe Text
  }
  deriving (Generic, Show)

mkArtist :: ArtistEntity -> [Text] -> Artist
mkArtist dbArtist names =
  Artist
    { key = dbArtist.id,
      artistIds = catMaybes [MusicBrainzArtistId <$> MB.MusicBrainzId <$> dbArtist.musicbrainz_id],
      name = dbArtist.name,
      aliases = names,
      disambiguation = dbArtist.disambiguation,
      biography = dbArtist.bio,
      shortBiography = dbArtist.short_bio,
      country = dbArtist.country >>= decodeAlphaTwo
    }

data ArtistId
  = MusicBrainzArtistId
      { musicBrainzArtistId :: MB.MusicBrainzId
      }
  | DiscogsArtistId
      { discogsArtistId :: Text
      }
  | SpotifyArtistId
      { spotifyArtistId :: Text
      }
  deriving (Show, Eq)

data NewArtist = NewArtist
  { name :: Text,
    country :: Maybe Country,
    disambiguation :: Maybe Text,
    bio :: Maybe Text,
    shortBio :: Maybe Text,
    musicBrainzId :: Maybe MB.MusicBrainzId
  }
  deriving (Generic, Eq, Ord, Show)

instance From NewArtist (ArtistTable Expr) where
  from a =
    ArtistTable
      { id = nullaryFunction "uuid_generate_v4",
        name = lit a.name,
        disambiguation = lit a.disambiguation,
        country = lit (alphaThreeLower <$> a.country),
        bio = lit a.bio,
        short_bio = lit a.shortBio,
        musicbrainz_id = lit (MB.mbid <$> a.musicBrainzId)
      }

mergeArtist :: ArtistEntity -> NewArtist -> ArtistEntity
mergeArtist artist newArtist =
  artist {
    name = newArtist.name,
    disambiguation = newArtist.disambiguation,
    country = newArtist.country <&> alphaThreeLower,
    bio = newArtist.bio,
    short_bio = newArtist.shortBio,
    musicbrainz_id = newArtist.musicBrainzId <&> (.mbid)
  }

mkNewArtist :: ArtistRef -> NewArtist -> ArtistEntity
mkNewArtist artistRef newArtist =
  ArtistTable {
    id = artistRef,
    name = newArtist.name,
    disambiguation = newArtist.disambiguation,
    country = newArtist.country <&> alphaThreeLower,
    bio = newArtist.bio,
    short_bio = newArtist.shortBio,
    musicbrainz_id = newArtist.musicBrainzId <&> (.mbid)
  }

instance From MB.Artist NewArtist where
  from a =
    NewArtist
      { name = a.name,
        disambiguation = a.disambiguation,
        country = (a.country >>= decodeAlphaTwo)
          <|> (a.area ^? _Just . (to (.iso3166_2codes)) . _Just . _head . (to (.country)) >>= decodeAlphaTwo)
          <|> (a.area ^? _Just . (to (.iso3166_1codes)) . _Just . _head >>= decodeAlphaTwo)
          <|> (a.beginArea ^? _Just . (to (.iso3166_2codes)) . _Just . _head . (to (.country)) >>= decodeAlphaTwo)
          <|> (a.beginArea ^? _Just . (to (.iso3166_1codes)) . _Just . _head >>= decodeAlphaTwo),
        bio = Nothing,
        shortBio = Nothing,
        musicBrainzId = Just a.id
      }
