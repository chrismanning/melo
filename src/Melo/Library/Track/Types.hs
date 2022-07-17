{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Melo.Library.Track.Types where

import Control.Lens hiding (from)
import Data.Int
import Data.Text
import Data.Time
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import Melo.Library.Album.Types
import Melo.Library.Source.Types
import Rel8
import Witch

data TrackTable f = TrackTable
  { id :: Column f UUID,
    title :: Column f Text,
    album_id :: Column f AlbumRef,
    track_number :: Column f (Maybe Int16),
    disc_number :: Column f (Maybe Int16),
    comment :: Column f (Maybe Text),
    source_id :: Column f SourceRef
--    length :: Column f CalendarDiffTime
  }
  deriving (Generic, Rel8able)

type Track = TrackTable Result

instance Entity (TrackTable Result) where
  type NewEntity (TrackTable Result) = NewTrack
  type PrimaryKey (TrackTable Result) = UUID
  primaryKey e = e.id

data NewTrack = NewTrack
  { title :: Text,
    trackNumber :: Maybe Int16,
    discNumber :: Maybe Int16,
    comment :: Maybe Text,
    sourceId :: SourceRef,
    albumId :: AlbumRef,
    length :: Maybe NominalDiffTime
  }
  deriving (Generic, Eq, Ord, Show)

instance From NewTrack (TrackTable Expr) where
  from t =
    TrackTable
      { id = nullaryFunction "uuid_generate_v4",
        title = lit $ t ^. #title,
        track_number = lit $ t ^. #trackNumber,
        comment = lit $ t ^. #comment,
        album_id = lit $ t ^. #albumId,
        disc_number = lit $ t ^. #discNumber,
        source_id = lit $ t ^. #sourceId
--        length = fromMaybe_ (lit (DB.Interval 0)) (lit (DB.Interval <$> t ^. #length))
      }
