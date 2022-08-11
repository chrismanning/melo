{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Artist.Staging.Types where

import Control.Lens hiding (from)
import Country
import Data.Hashable
import Data.Morpheus.Types as M
import Data.Morpheus.Kind
import Data.Text
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import Melo.Lookup.MusicBrainz as MB
import Melo.Library.Artist.Types
import Rel8
import Witch

data ArtistStageTable f = ArtistStageTable
  { id :: Column f StagedArtistRef,
    name :: Column f Text,
    disambiguation :: Column f (Maybe Text),
    short_bio :: Column f (Maybe Text),
    bio :: Column f (Maybe Text),
    country :: Column f (Maybe Text),
    musicbrainz_id :: Column f (Maybe Text),
    ref_artist_id :: Column f (Maybe UUID),
    ref_album_id :: Column f (Maybe UUID),
    ref_track_id :: Column f (Maybe UUID)
  }
  deriving (Generic, Rel8able)

instance Entity (ArtistStageTable Result) where
  type NewEntity (ArtistStageTable Result) = NewStagedArtist
  type PrimaryKey (ArtistStageTable Result) = StagedArtistRef
  primaryKey e = e.id

newtype StagedArtistRef = StagedArtistRef UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)

instance GQLType StagedArtistRef where
  type KIND StagedArtistRef = SCALAR

instance EncodeScalar StagedArtistRef where
  encodeScalar (StagedArtistRef uuid) = M.String $ toText uuid

instance DecodeScalar StagedArtistRef where
  decodeScalar (M.String s) = case fromText s of
    Nothing -> Left "StagedArtistRef must be UUID"
    Just uuid -> Right $ StagedArtistRef uuid
  decodeScalar _ = Left "StagedArtistRef must be a String"

instance From StagedArtistRef UUID where
  from (StagedArtistRef uuid) = uuid

data StagedArtist = StagedArtist
  { key :: StagedArtistRef,
    artistIds :: Maybe [ArtistRef],
    name :: Maybe Text,
    disambiguation :: Maybe Text,
    country :: Maybe Country,
    biography :: Maybe Text,
    shortBiography :: Maybe Text
  }
  deriving (Generic, Show)

instance From (ArtistStageTable Result) StagedArtist where
  from dbArtist =
    StagedArtist
      { key = dbArtist ^. #id,
        artistIds = Nothing,
        name = Just (dbArtist ^. #name),
        disambiguation = dbArtist ^. #disambiguation,
        biography = dbArtist ^. #bio,
        shortBiography = dbArtist ^. #short_bio,
        country = dbArtist ^. #country >>= decodeAlphaTwo
      }

data NewStagedArtist = NewStagedArtist
  { name :: Text,
    country :: Maybe Country,
    disambiguation :: Maybe Text,
    bio :: Maybe Text,
    shortBio :: Maybe Text
  }
  deriving (Generic, Eq, Ord, Show)

instance From MB.Artist NewStagedArtist where
  from a =
    NewStagedArtist
      { name = a ^. #name,
        disambiguation = a ^. #disambiguation,
        country = a ^. #country >>= decodeAlphaTwo,
        bio = Nothing,
        shortBio = Nothing
      }

instance From NewStagedArtist (ArtistStageTable Expr) where
  from a =
    ArtistStageTable
      { id = nullaryFunction "uuid_generate_v4",
        name = lit (a ^. #name),
        disambiguation = lit (a ^. #disambiguation),
        country = lit (alphaThreeLower <$> a ^. #country),
        bio = lit (a ^. #bio),
        short_bio = lit (a ^. #shortBio),
        ref_artist_id = lit Nothing,
        ref_album_id = lit Nothing,
        ref_track_id = lit Nothing,
        musicbrainz_id = lit Nothing
      }
