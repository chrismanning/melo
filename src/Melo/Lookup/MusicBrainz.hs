{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Lookup.MusicBrainz
  ( MusicBrainzId (..),
    runMusicBrainzServiceIO,
    MusicBrainzService (..),
    MusicBrainzServiceIOC (..),
    searchArtists,
    Artist (..),
    ArtistCredit (..),
    ArtistSearch (..),
    searchRecordings,
    Recording (..),
    RecordingSearch (..),
    searchReleases,
    Release (..),
    ReleaseSearch (..),
    searchReleaseGroups,
    ReleaseGroup (..),
    artistIdTag,
    originalArtistIdTag,
    albumArtistIdTag,
    trackIdTag,
    releaseTrackIdTag,
    releaseGroupIdTag,
  )
where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.TokenLimiter
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.Sum
import Control.Effect.Error
import Control.Lens hiding (lens)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger (LoggingT)
import Country
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.Aeson.Types as A
import Data.Default
import Data.Functor
import Data.Generics.Labels
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock as Clock
import GHC.Generics (Generic, Generic1)
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Common.RateLimit
import Melo.Format.Mapping
import Network.HTTP.Types
import qualified Network.Wreq as Wr

newtype MusicBrainzId
  = MusicBrainzId
      { mbid :: Text
      }
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

newtype ArtistSearch
  = ArtistSearch
      { artist :: Text
      }
  deriving (Generic)

newtype ArtistSearchResult
  = ArtistSearchResult
      { artists :: [Artist]
      }
  deriving (Generic)
  deriving anyclass (FromJSON)

data Artist
  = Artist
      { id :: MusicBrainzId,
        name :: Text,
        sortName :: Maybe Text,
        country :: Maybe Text,
        score :: Maybe Int
      }
  deriving (Show, Generic)

instance FromJSON Artist where
  parseJSON = genericParseJSON mbAesonOptions

data ReleaseSearch
  = ReleaseSearch
      { albumArtist :: Maybe Text,
        albumTitle :: Maybe Text
      }
  deriving (Generic, Default)

newtype ReleaseSearchResult
  = ReleaseSearchResult
      { releases :: [Release]
      }
  deriving (Generic)
  deriving anyclass (FromJSON)

data Release
  = Release
      { id :: MusicBrainzId,
        title :: Text,
        artistCredit :: [ArtistCredit],
        date :: Maybe Text
      }
  deriving (Show, Generic)

instance FromJSON Release where
  parseJSON = genericParseJSON mbAesonOptions

data ArtistCredit
  = ArtistCredit
      { name :: Maybe Text,
        artist :: Artist
      }
  deriving (Show, Generic)

instance FromJSON ArtistCredit where
  parseJSON = genericParseJSON mbAesonOptions

newtype ReleaseGroupSearchResult
  = ReleaseGroupSearchResult
      { releaseGroups :: [ReleaseGroup]
      }
  deriving (Generic)
  deriving anyclass (FromJSON)

data ReleaseGroup
  = ReleaseGroup
      { id :: MusicBrainzId,
        title :: Text,
        artistCredit :: [ArtistCredit]
      }
  deriving (Show, Generic)

instance FromJSON ReleaseGroup where
  parseJSON = genericParseJSON mbAesonOptions

data RecordingSearch
  = RecordingSearch
      {
      }

newtype RecordingSearchResult
  = RecordingSearchResult
      { recordings :: [Recording]
      }
  deriving (Generic)
  deriving anyclass (FromJSON)

data Recording
  = Recording
      {
      }
  deriving (Show, Generic)

instance FromJSON Recording where
  parseJSON = genericParseJSON mbAesonOptions

data MusicBrainzService (m :: Type -> Type) k where
  SearchReleases :: ReleaseSearch -> MusicBrainzService m [Release]
  SearchReleaseGroups :: ReleaseSearch -> MusicBrainzService m [ReleaseGroup]
  SearchArtists :: ArtistSearch -> MusicBrainzService m [Artist]
  SearchRecordings :: RecordingSearch -> MusicBrainzService m [Recording]
  GetArtist :: MusicBrainzId -> MusicBrainzService m (Maybe Artist)
  GetArtistReleaseGroups :: MusicBrainzId -> MusicBrainzService m [ReleaseGroup]
  GetArtistReleases :: MusicBrainzId -> MusicBrainzService m [Release]

searchReleases :: Has MusicBrainzService sig m => ReleaseSearch -> m [Release]
searchReleases s = send (SearchReleases s)

searchReleaseGroups :: Has MusicBrainzService sig m => ReleaseSearch -> m [ReleaseGroup]
searchReleaseGroups s = send (SearchReleaseGroups s)

searchArtists :: Has MusicBrainzService sig m => ArtistSearch -> m [Artist]
searchArtists s = send (SearchArtists s)

searchRecordings :: Has MusicBrainzService sig m => RecordingSearch -> m [Recording]
searchRecordings s = send (SearchRecordings s)

newtype MusicBrainzServiceIOC m a
  = MusicBrainzServiceIOC
      { runMusicBrainzServiceIOC :: RateLimitIOC m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  ( Has (Lift IO) sig m,
    Has Http sig m,
    Has Logging sig m,
    Algebra sig m
  ) =>
  Algebra (MusicBrainzService :+: sig) (MusicBrainzServiceIOC m)
  where
  alg _ (L mb) ctx = MusicBrainzServiceIOC $
    waitReady >> case mb of
      SearchReleases search -> do
        let qterms =
              catMaybes
                [ fmap (\t -> "release:\"" <> t <> "\"") $ search ^. #albumTitle,
                  fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #albumArtist
                ]
        case qterms of
          [] -> (ctx $>) <$> pure []
          ts -> do
            let q = T.intercalate " AND " ts
            let opts = mbWreqDefaults & Wr.param "query" .~ [q]
            let url = baseUrl <> "/release"
            r' :: Wr.Response ReleaseSearchResult <- getWithJson opts url
            (ctx $>) <$> pure (r' ^. Wr.responseBody . #releases)
      SearchReleaseGroups search -> do
        let qterms =
              catMaybes
                [ fmap (\t -> "release:\"" <> t <> "\"") $ search ^. #albumTitle,
                  fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #albumArtist
                ]
        case qterms of
          [] -> (ctx $>) <$> pure []
          ts -> do
            let q = T.intercalate " AND " ts
            let opts = mbWreqDefaults & Wr.param "query" .~ [q]
            let url = baseUrl <> "/release-group"
            r' :: Wr.Response ReleaseGroupSearchResult <- getWithJson opts url
            (ctx $>) <$> pure (r' ^. Wr.responseBody . #releaseGroups)
      SearchArtists search -> do
        let opts =
              mbWreqDefaults
                & Wr.param "query" .~ ["artist:\"" <> search ^. #artist <> "\""]
        let url = baseUrl <> "/artist"
        r :: Wr.Response ArtistSearchResult <- getWithJson opts url
        (ctx $>) <$> pure (r ^. Wr.responseBody . #artists)
      GetArtist artistId -> do
        undefined
  alg hdl (R other) ctx = MusicBrainzServiceIOC $
    alg (runMusicBrainzServiceIOC . hdl) (R other) ctx

mbWreqDefaults :: Wr.Options
mbWreqDefaults =
  Wr.defaults
    & Wr.header "User-Agent" .~ [meloUserAgent]
    & Wr.header "Accept" .~ ["application/json"]

baseUrl :: IsString s => s
baseUrl = "http://musicbrainz.org/ws/2"

runMusicBrainzServiceIO :: (Has (Lift IO) sig m) => MusicBrainzServiceIOC m a -> m a
runMusicBrainzServiceIO m = runRateLimitIO mbRateLimitConfig $ runMusicBrainzServiceIOC m

mbRateLimitConfig :: LimitConfig
mbRateLimitConfig =
  defaultLimitConfig
    { maxBucketTokens = 1,
      initialBucketTokens = 1,
      bucketRefillTokensPerSecond = 1
    }

mbAesonOptions :: A.Options
mbAesonOptions = defaultOptions {fieldLabelModifier = trainCase}

artistIdTag :: TagMapping
artistIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ARTISTID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ARTISTID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Artist Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Artist Id"
      }

originalArtistIdTag :: TagMapping
originalArtistIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ORIGINALARTISTID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ORIGINALARTISTID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Original Artist Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Original Artist Id"
      }

albumArtistIdTag :: TagMapping
albumArtistIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMARTISTID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMARTISTID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Album Artist Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Album Artist Id"
      }

trackIdTag :: TagMapping
trackIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_TRACKID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_TRACKID",
        id3v2_3 = caseInsensitiveMapping "UFID://musicbrainz.org",
        id3v2_4 = caseInsensitiveMapping "UFID://musicbrainz.org"
      }

releaseTrackIdTag :: TagMapping
releaseTrackIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_RELEASETRACKID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_RELEASETRACKID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Release Track Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Release Track Id"
      }

releaseGroupIdTag :: TagMapping
releaseGroupIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_RELEASEGROUPID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_RELEASEGROUPID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Release Group Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Release Group Id"
      }
