{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Service where

import Basement.From
import Control.Algebra
import Control.Carrier.Cull.Church
import Control.Carrier.NonDet.Church
import Control.Effect.Cull
import Control.Effect.Empty as E
import Control.Effect.NonDet
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.IO.Class
import Country
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

data Artist
  = Artist
      { artistIds :: [ArtistId],
        name :: Text,
        country :: Maybe Country
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
  from artist =
    Artist
      { artistIds = [MusicBrainzArtistId (artist ^. #id)],
        name = artist ^. #name,
        country = artist ^. #country >>= decodeAlphaTwo
      }

data ArtistService :: Effect where
  IdentifyArtists :: Metadata -> ArtistService m [Artist]
  SetArtists :: Metadata -> [Artist] -> ArtistService m Metadata

identifyArtists :: Has ArtistService sig m => Metadata -> m [Artist]
identifyArtists m = send (IdentifyArtists m)

setArtists :: Has ArtistService sig m => Metadata -> [Artist] -> m Metadata
setArtists m a = send (SetArtists m a)

newtype ArtistServiceIOC m a
  = ArtistServiceIOC
      { runArtistServiceIOC :: m a
      }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Algebra sig m,
    Has MetadataService sig m,
    Has MB.MusicBrainzService sig m,
    Has Logging sig m
  ) =>
  Algebra (ArtistService :+: sig) (ArtistServiceIOC m)
  where
  alg _hdl (L sig) ctx =
    case sig of
      IdentifyArtists m -> do
        let tag = lens m
        let ts = m ^. #tags
        r <-
          runCullM id $ cull $
            getArtistByMusicBrainzId (ts ^. tag MB.artistIdTag)
              <|> getArtistByAlbum (ts ^. tag M.album ^? _head) (ts ^. tag M.albumArtist ^? _head)
              <|> getArtistByTrackArtistName (ts ^. tag M.artist ^? _head)
        $(logDebugShow) r
        (ctx $>) <$> pure r
      -- TODO search Discogs (https://www.discogs.com/developers/#page:database)
      -- TODO search Spotify (https://developer.spotify.com/documentation/web-api/reference/search/search/)
      -- TODO search Rovi (http://developer.rovicorp.com/docs)
      SetArtists m a -> do
        let tag = lens m
        let ts = m ^. #tags
        let m' = m {tags = ts & tag M.trackArtistTag .~ (a <&> (^. #name))}
        (ctx $>) <$> pure m'
  alg hdl (R other) ctx = ArtistServiceIOC (alg (runArtistServiceIOC . hdl) other ctx)

getArtistByMusicBrainzId ::
  ( Has MB.MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  [Text] ->
  m [Artist]
getArtistByMusicBrainzId [] = E.empty
getArtistByMusicBrainzId artistIds = do
  artists <- catMaybes <$> forM artistIds (MB.getArtist . MB.MusicBrainzId)
  $(logDebugShow) artists
  case artists of
    [] -> E.empty
    _ -> pure $ fmap from artists

getArtistByAlbum ::
  ( Has MB.MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  Maybe Text ->
  Maybe Text ->
  m [Artist]
getArtistByAlbum Nothing Nothing = E.empty
getArtistByAlbum albumTitle albumArtist = do
  releases <- MB.searchReleases def {MB.albumArtist = albumArtist, MB.albumTitle = albumTitle}
  $(logDebugShow) releases
  let releases' = filter (\release -> release ^. #score == Just 100) releases
  let artists = releases' ^.. traverse . #artistCredit . traverse . #artist . #id . #mbid
  getArtistByMusicBrainzId artists

getArtistByTrackArtistName ::
  ( Has MB.MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  Maybe Text ->
  m [Artist]
getArtistByTrackArtistName Nothing = E.empty
getArtistByTrackArtistName (Just trackArtist) = do
  artists <- MB.searchArtists MB.ArtistSearch {MB.artist = trackArtist}
  $(logDebugShow) artists
  case artists of
    [] -> E.empty
    _ -> pure $ fmap from artists

runArtistServiceIO :: ArtistServiceIOC m a -> m a
runArtistServiceIO = runArtistServiceIOC
