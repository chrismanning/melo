{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Melo.Library.Artist.Types where

import Control.Lens hiding (from, lens)
import Country
import Data.Hashable
import Data.Morpheus.Types as M
import Data.Morpheus.Kind
import Data.Text (Text)
import Data.UUID
import GHC.Generics
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

instance Entity (ArtistTable Result) where
  type NewEntity (ArtistTable Result) = NewArtist
  type PrimaryKey (ArtistTable Result) = ArtistRef

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
    disambiguation :: Maybe Text,
    country :: Maybe Country,
    biography :: Maybe Text,
    shortBiography :: Maybe Text
  }
  deriving (Generic, Show)

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

instance From (ArtistTable Result) Artist where
  from dbArtist =
    Artist
      { key = dbArtist ^. #id,
        artistIds = [],
        name = dbArtist ^. #name,
        disambiguation = dbArtist ^. #disambiguation,
        biography = dbArtist ^. #bio,
        shortBiography = dbArtist ^. #short_bio,
        country = dbArtist ^. #country >>= decodeAlphaTwo
      }

data NewArtist = NewArtist
  { name :: Text,
    country :: Maybe Country,
    disambiguation :: Maybe Text,
    bio :: Maybe Text,
    shortBio :: Maybe Text
  }
  deriving (Generic, Eq, Ord, Show)

instance From NewArtist (ArtistTable Expr) where
  from a =
    ArtistTable
      { id = nullaryFunction "uuid_generate_v4",
        name = lit (a ^. #name),
        disambiguation = lit (a ^. #disambiguation),
        country = lit (alphaThreeLower <$> a ^. #country),
        bio = lit (a ^. #bio),
        short_bio = lit (a ^. #shortBio),
        musicbrainz_id = lit Nothing
      }

instance From MB.Artist NewArtist where
  from a =
    NewArtist
      { name = a ^. #name,
        disambiguation = a ^. #disambiguation,
        country = a ^. #country >>= decodeAlphaTwo,
        bio = Nothing,
        shortBio = Nothing
      }

data Reviewed a
  = CommitAsIs
  | CommitAs a
  | DoNotCommit
