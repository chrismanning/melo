{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Control.Algebra
import Control.Effect.Sum
import Control.Effect.Reader
import Control.Lens hiding (lens)
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics (Generic, Generic1)
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Metadata.Service
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
      { musicBrainzArtistId :: Text
      }
  | DiscogsArtistId
      { discogsArtistId :: Text
      }
  | SpotifyArtistId
      { spotifyArtistId :: Text
      }
  deriving (Show, Eq)

-- TODO ArtistService - identify artists -- spotify, discogs, musicbrainz
data ArtistService m k
  = IdentifyArtists MetadataFile ([Artist] -> m k)
  deriving (Functor, Generic1, HFunctor, Effect)

identifyArtists :: Has ArtistService sig m => MetadataFile -> m [Artist]
identifyArtists f = send (IdentifyArtists f pure)

newtype ArtistServiceIOC m a
  = ArtistServiceIOC
      { runArtistServiceIOC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig,
    Has MetadataService sig m, Has (Reader Session) sig m) =>
  Algebra (ArtistService :+: sig) (ArtistServiceIOC m)
  where
  alg = \case
    L (IdentifyArtists f k) -> do
      m' <- chooseMetadata (H.elems $ f ^. #metadata)
      case m' of
        Just m -> do
          let tag = lens m
          let ts = m ^. #tags
          let (albumArtist : _) = ts ^. tag M.albumArtist
          let (trackArtist : _) = ts ^. tag M.artist
          let (albumTitle : _) = ts ^. tag M.album
          -- "http://musicbrainz.org/ws/2/release/?query=release%3A%22De%E2%80%90Loused%20in%20the%20Comatorium%22&fmt=json"
          let opts = defaults
                & header "User-Agent" .~ ["melo/0.1.0.0 ( https://github.com/chrismanning/melo )"]
                & header "Accept" .~ ["application/json"]
                & param "query" .~
                  ["artist:\"" <> albumArtist <> "\" AND release:\"" <> albumTitle <> "\""]
          let url = "http://musicbrainz.org/ws/2/release-group/"
          s <- ask
          r <- liftIO $ Sess.getWith opts s url
          liftIO $ print (r ^. responseBody)
          -- TODO search MusicBrainz (https://musicbrainz.org/doc/Development/XML_Web_Service/Version_2/Search)
          -- TODO search Discogs (https://www.discogs.com/developers/#page:database)
          -- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
          -- TODO search Rovi (http://developer.rovicorp.com/docs)
          k []
        Nothing -> k []
    R other -> ArtistServiceIOC (alg (handleCoercible other))

runArtistServiceIO :: ArtistServiceIOC m a -> m a
runArtistServiceIO = runArtistServiceIOC
