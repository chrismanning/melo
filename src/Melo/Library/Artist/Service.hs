{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Control.Algebra
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Lens hiding (lens)
import Control.Monad.IO.Class
import Data.ByteString
import Data.Default
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics (Generic, Generic1)
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Metadata.Service
import qualified Melo.Lookup.MusicBrainz as MB
--import Network.HTTP.Client
import Network.HTTP.Types
import Network.Wreq
import Network.Wreq.Cache
import qualified Network.Wreq.Session as Sess
import Network.Wreq.Session (Session)

data Artist
  = Artist
      { artistId :: [ArtistId],
        name :: Text
      }
  deriving (Generic)

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

-- TODO ArtistService - identify artists -- spotify, discogs, musicbrainz
data ArtistService :: Effect where
  IdentifyArtists :: MetadataFile -> ArtistService m [Artist]

identifyArtists :: Has ArtistService sig m => MetadataFile -> m [Artist]
identifyArtists f = send (IdentifyArtists f)

newtype ArtistServiceIOC m a
  = ArtistServiceIOC
      { runArtistServiceIOC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  ( MonadIO m,
    Algebra sig m,
    Has MetadataService sig m,
    Has MB.MusicBrainzService sig m,
    Has Logging sig m
  ) =>
  Algebra (ArtistService :+: sig) (ArtistServiceIOC m)
  where
  alg hdl sig ctx = case sig of
    L (IdentifyArtists f) -> do
      m' <- chooseMetadata (H.elems $ f ^. #metadata)
      case m' of
        Just m -> do
          let tag = lens m
          let ts = m ^. #tags
          let albumArtist = ts ^. tag M.albumArtist ^? _head
          let trackArtist = ts ^. tag M.artist ^? _head
          let trackTitle = ts ^. tag M.trackTitle ^? _head
          --          let albumTitle = ts ^. tag M.album ^? _head
          -- first try identifying the artist via the album
          case ts ^. tag M.album ^? _head of
            Just albumTitle -> do
              releases <- MB.searchReleases def {MB.albumArtist = albumArtist, MB.albumTitle = Just albumTitle}
              $(logDebugShow) releases
              pure ()
            Nothing ->
              case trackArtist of
                Just a -> do
                  artists <- MB.searchArtists MB.ArtistSearch {MB.artist = a}
                  $(logDebugShow) artists
                Nothing -> pure ()
          -- TODO search MusicBrainz (https://musicbrainz.org/doc/Development/XML_Web_Service/Version_2/Search)
          -- TODO search Discogs (https://www.discogs.com/developers/#page:database)
          -- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
          -- TODO search Rovi (http://developer.rovicorp.com/docs)
          (ctx $>) <$> pure []
        Nothing -> (ctx $>) <$> pure []
    R other -> ArtistServiceIOC (alg (runArtistServiceIOC . hdl) other ctx)

runArtistServiceIO :: ArtistServiceIOC m a -> m a
runArtistServiceIO = runArtistServiceIOC
