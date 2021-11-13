{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Melo.Library.Album.Types where

import Control.Lens hiding (from, lens)
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Hashable
import Data.Text (Text)
import Data.Time
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import qualified Melo.Lookup.MusicBrainz as MB
import Rel8
import Witch

data AlbumTable f = AlbumTable
  { id :: Column f AlbumRef,
    title :: Column f Text,
    comment :: Column f (Maybe Text),
    year_released :: Column f (Maybe Text),
    length :: Column f CalendarDiffTime,
    musicbrainz_id :: Column f (Maybe Text)
  }
  deriving (Generic, Rel8able)

instance Entity (AlbumTable Result) where
  type NewEntity (AlbumTable Result) = NewAlbum
  type PrimaryKey (AlbumTable Result) = AlbumRef

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
    title :: Text
  }
  deriving (Generic)

instance From (AlbumTable Result) Album where
  from dbAlbum =
    Album
      { dbId = dbAlbum ^. #id,
        title = dbAlbum ^. #title
      }

data NewAlbum = NewAlbum
  { title :: Text,
    comment :: Maybe Text,
    yearReleased :: Maybe Text,
    length :: NominalDiffTime
  }
  deriving (Generic, Eq, Ord, Show)

instance From MB.Release NewAlbum where
  from mbRelease =
    NewAlbum
      { title = mbRelease ^. #title,
        yearReleased = mbRelease ^. #date,
        comment = Nothing,
        length = 0
      }

instance From NewAlbum (AlbumTable Expr) where
  from a =
    AlbumTable
      { id = nullaryFunction "uuid_generate_v4",
        title = lit (a ^. #title),
        comment = lit Nothing,
        year_released = lit (a ^. #yearReleased),
        length = lit (calendarTimeTime $ a ^. #length),
        musicbrainz_id = lit Nothing
      }
