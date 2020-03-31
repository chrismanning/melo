{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Basement.From
import Control.Algebra
import Control.Carrier.NonDet.Church
import Control.Carrier.Cull.Church
import Control.Effect.Cull
import Control.Effect.NonDet
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Traversable
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

instance From MB.Artist Artist where
  from artist = Artist [MusicBrainzArtistId (artist ^. #id)] (artist ^. #name)

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
  (Algebra sig m, Has MetadataService sig m, Has MB.MusicBrainzService sig m, Has Logging sig m) =>
  Algebra (ArtistService :+: sig) (ArtistServiceIOC m)
  where
  alg hdl sig ctx =
    case sig of
      L (IdentifyArtists f) -> do
        m' <- chooseMetadata (H.elems $ f ^. #metadata)
        case m' of
          Just m -> do
            let tag = lens m
            let ts = m ^. #tags
            r <- runCullM id $
              getArtistByMusicBrainzId (ts ^. tag MB.artistIdTag)
                <|> getArtistByAlbum (ts ^. tag M.album ^? _head) (ts ^. tag M.albumArtist ^? _head)
                <|> getArtistByTrackArtistName (ts ^. tag M.artist ^? _head)
            $(logDebugShow) r
            (ctx $>) <$> pure r
          -- TODO search Discogs (https://www.discogs.com/developers/#page:database)
          -- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
          -- TODO search Rovi (http://developer.rovicorp.com/docs)
          Nothing -> (ctx $>) <$> pure []
      R other -> ArtistServiceIOC (alg (runArtistServiceIOC . hdl) other ctx)

getArtistByMusicBrainzId :: (Has MB.MusicBrainzService sig m, Has Logging sig m) => [Text] -> m [Artist]
getArtistByMusicBrainzId artistIds = do
  artists <- catMaybes <$> forM artistIds (MB.getArtist . MB.MusicBrainzId)
  $(logDebugShow) artists
  pure $ fmap from artists

getArtistByAlbum :: (Has MB.MusicBrainzService sig m, Has Logging sig m) => Maybe Text -> Maybe Text -> m [Artist]
getArtistByAlbum albumTitle albumArtist = do
  releases <- MB.searchReleases def {MB.albumArtist = albumArtist, MB.albumTitle = albumTitle}
  $(logDebugShow) releases
  let releases' :: [MB.Release] = filter (\release -> release ^. #score == Just 100) releases
  let artists = releases' ^.. traverse . #artistCredit . traverse . #artist
  pure $ fmap from artists

getArtistByTrackArtistName :: (Has MB.MusicBrainzService sig m, Has Logging sig m) => Maybe Text -> m [Artist]
getArtistByTrackArtistName Nothing = pure empty
getArtistByTrackArtistName (Just trackArtist) = do
  artists <- MB.searchArtists MB.ArtistSearch {MB.artist = trackArtist}
  $(logDebugShow) artists
  pure $ fmap from artists

runArtistServiceIO :: ArtistServiceIOC m a -> m a
runArtistServiceIO = runArtistServiceIOC
