{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Track.Types where

import Data.Hashable
import Data.Int
import Data.Time
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import Melo.Library.Release.Types
import Melo.Library.Artist.Name.Types
import Melo.Library.Source.Types
import Melo.Lookup.MusicBrainz as MB
import Rel8

data TrackTable f = TrackTable
  { id :: Column f TrackRef,
    title :: Column f Text,
    release_id :: Column f ReleaseRef,
    track_number :: Column f Int16,
    disc_number :: Column f (Maybe Int16),
    comment :: Column f (Maybe Text),
    source_id :: Column f SourceRef,
    length :: Column f CalendarDiffTime,
    musicbrainz_id :: Column f (Maybe Text)
  }
  deriving (Generic, Rel8able)

type TrackEntity = TrackTable Result

deriving instance Show TrackEntity
deriving via (FromGeneric TrackEntity) instance TextShow TrackEntity

deriving instance Eq TrackEntity

instance Entity TrackEntity where
  type NewEntity TrackEntity = NewTrack
  type PrimaryKey TrackEntity = TrackRef
  primaryKey e = e.id

newtype TrackRef = TrackRef UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)
  deriving TextShow via FromGeneric TrackRef

data NewTrack = NewTrack
  { title :: Text,
    trackNumber :: Int16,
    discNumber :: Maybe Int16,
    comment :: Maybe Text,
    sourceId :: SourceRef,
    releaseId :: ReleaseRef,
    length :: NominalDiffTime,
    musicBrainzId :: Maybe MB.MusicBrainzId
  }
  deriving (Generic, Eq, Ord, Show)
  deriving TextShow via FromGeneric NewTrack

instance From NewTrack (TrackTable Expr) where
  from t =
    TrackTable
      { id = nullaryFunction "uuid_generate_v4",
        title = lit t.title,
        track_number = lit t.trackNumber,
        comment = lit t.comment,
        release_id = lit t.releaseId,
        disc_number = lit t.discNumber,
        source_id = lit t.sourceId,
        length = lit (calendarTimeTime t.length),
        musicbrainz_id = lit (MB.mbid <$> t.musicBrainzId)
      }

fromNewTrack :: NewTrack -> UUID -> TrackEntity
fromNewTrack t id =
  TrackTable
    { id = TrackRef id,
      title = t.title,
      track_number = t.trackNumber,
      disc_number = t.discNumber,
      comment = t.comment,
      source_id = t.sourceId,
      release_id = t.releaseId,
      length = calendarTimeTime t.length,
      musicbrainz_id = t.musicBrainzId <&> (.mbid)
    }

data Track = Track
  { ref :: TrackRef,
    title :: Text,
    artists :: [ArtistNameEntity],
    trackNumber :: Int16,
    discNumber :: Maybe Int16,
    comment :: Maybe Text,
    sourceRef :: SourceRef,
    releaseRef :: ReleaseRef,
    length :: NominalDiffTime,
    musicBrainzId :: Maybe MB.MusicBrainzId
  }
  deriving (Generic, Eq, Ord, Show)
  deriving TextShow via FromGeneric Track

mkTrack :: Foldable f => f ArtistNameEntity -> TrackEntity -> Track
mkTrack artists t =
  Track
    { ref = t.id,
      title = t.title,
      trackNumber = t.track_number,
      discNumber = t.disc_number,
      comment = t.comment,
      sourceRef = t.source_id,
      releaseRef = t.release_id,
      length = ctTime t.length,
      musicBrainzId = MB.MusicBrainzId <$> t.musicbrainz_id,
      artists = toList artists
    }
