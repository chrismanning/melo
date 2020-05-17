module Melo.Library.Album.Types where

import Basement.From
import Control.Lens hiding (from, lens)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Query
import GHC.Generics
import qualified Melo.Database.Model as DB
import qualified Melo.Lookup.MusicBrainz as MB

data Album = Album
  { dbId :: DB.AlbumKey,
    title :: Text
  }
  deriving (Generic)

instance From DB.Album Album where
  from dbAlbum =
    Album
      { dbId = DB.AlbumKey (dbAlbum ^. #id),
        title = dbAlbum ^. #title
      }

data NewAlbum = NewAlbum
  { title :: Text,
    comment :: Maybe Text,
    yearReleased :: Maybe Text,
    length :: NominalDiffTime
  }
  deriving (Generic, Eq, Show)

instance From MB.Release NewAlbum where
  from mbRelease =
    NewAlbum
      { title = mbRelease ^. #title,
        yearReleased = mbRelease ^. #date,
        comment = Nothing,
        length = 0
      }

instance From NewAlbum (DB.AlbumT (QExpr Postgres s)) where
  from a =
    DB.Album
      { id = default_,
        title = val_ (a ^. #title),
        comment = nothing_,
        year_released = val_ (a ^. #yearReleased),
        length = val_ (DB.Interval $ a ^. #length)
      }
