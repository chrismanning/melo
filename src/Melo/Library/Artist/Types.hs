module Melo.Library.Artist.Types where

import Basement.From
import Control.Lens hiding (from, lens)
import Country
import Data.Text (Text)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Query
import GHC.Generics
import qualified Melo.Library.Database.Model as DB
import qualified Melo.Lookup.MusicBrainz as MB

--newtype Staging a = Staging {
--  unStage :: a
--}

data Artist = Artist
  { dbId :: DB.ArtistKey,
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

instance From DB.Artist Artist where
  from dbArtist =
    Artist
      { dbId = DB.ArtistKey (dbArtist ^. #id),
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
  deriving (Generic, Eq, Show)

instance From MB.Artist NewArtist where
  from a =
    NewArtist
      { name = a ^. #name,
        disambiguation = a ^. #disambiguation,
        country = a ^. #country >>= decodeAlphaTwo,
        bio = Nothing,
        shortBio = Nothing
      }

instance From NewArtist (DB.ArtistT (QExpr Postgres s)) where
  from a =
    DB.Artist
      { id = default_,
        name = val_ (a ^. #name),
        disambiguation = val_ (a ^. #disambiguation),
        country = val_ (alphaThreeLower <$> a ^. #country),
        bio = val_ (a ^. #bio),
        short_bio = val_ (a ^. #shortBio)
      }
