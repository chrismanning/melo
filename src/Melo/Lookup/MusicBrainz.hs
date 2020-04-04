{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Lookup.MusicBrainz
  ( MusicBrainzId (..),
    runMusicBrainzServiceIO,
    MusicBrainzService (..),
    MusicBrainzServiceIOC (..),
    getArtist,
    searchArtists,
    Artist (..),
    ArtistCredit (..),
    ArtistSearch (..),
    searchRecordings,
    Recording (..),
    RecordingSearch (..),
    searchReleases,
    getArtistReleases,
    Release (..),
    ReleaseSearch (..),
    searchReleaseGroups,
    getArtistReleaseGroups,
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
import Control.Carrier.Empty.Church as E
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.TokenLimiter
import Control.Lens hiding (lens)
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.Default
import Data.Functor
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Common.RateLimit
import Melo.Format.Mapping
import qualified Network.Wreq as Wr
import qualified Network.Wreq.Session as WrS

newtype MusicBrainzId
  = MusicBrainzId
      { mbid :: Text
      }
  deriving (Show, Eq, Ord, Generic)
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
  deriving (Show, Generic)
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
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Release
  = Release
      { id :: MusicBrainzId,
        title :: Text,
        artistCredit :: [ArtistCredit],
        date :: Maybe Text,
        score :: Maybe Int
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
  deriving (Show, Generic)
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
      { title :: Maybe Text,
        artist :: Maybe Text,
        album :: Maybe Text
      }
  deriving (Show, Generic)

newtype RecordingSearchResult
  = RecordingSearchResult
      { recordings :: [Recording]
      }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Recording
  = Recording
      { id :: MusicBrainzId,
        score :: Int,
        title :: Text,
        artistCredit :: [ArtistCredit],
        releases :: [Release]
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
  GetArtistRecordings :: MusicBrainzId -> MusicBrainzService m [Recording]

searchReleases :: Has MusicBrainzService sig m => ReleaseSearch -> m [Release]
searchReleases s = send (SearchReleases s)

searchReleaseGroups :: Has MusicBrainzService sig m => ReleaseSearch -> m [ReleaseGroup]
searchReleaseGroups s = send (SearchReleaseGroups s)

searchArtists :: Has MusicBrainzService sig m => ArtistSearch -> m [Artist]
searchArtists s = send (SearchArtists s)

searchRecordings :: Has MusicBrainzService sig m => RecordingSearch -> m [Recording]
searchRecordings s = send (SearchRecordings s)

getArtist :: Has MusicBrainzService sig m => MusicBrainzId -> m (Maybe Artist)
getArtist mbid = send (GetArtist mbid)

getArtistReleaseGroups :: Has MusicBrainzService sig m => MusicBrainzId -> m [ReleaseGroup]
getArtistReleaseGroups mbid = send (GetArtistReleaseGroups mbid)

getArtistReleases :: Has MusicBrainzService sig m => MusicBrainzId -> m [Release]
getArtistReleases mbid = send (GetArtistReleases mbid)

newtype MusicBrainzServiceIOC m a
  = MusicBrainzServiceIOC
      { runMusicBrainzServiceIOC :: HttpSessionIOC (RateLimitIOC m) a
      }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has (Lift IO) sig m,
    Has Logging sig m
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
            r <- do
              r' :: Wr.Response ReleaseSearchResult <- getWithJson opts url
              pure (r' ^. Wr.responseBody . #releases)
            (ctx $>) <$> pure r
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
            r <- do
              r' :: Wr.Response ReleaseGroupSearchResult <- getWithJson opts url
              pure (r' ^. Wr.responseBody . #releaseGroups)
            (ctx $>) <$> pure r
      SearchArtists search -> do
        let opts =
              mbWreqDefaults
                & Wr.param "query" .~ ["artist:\"" <> search ^. #artist <> "\""]
        let url = baseUrl <> "/artist"
        r <- do
          r' :: Wr.Response ArtistSearchResult <- getWithJson opts url
          pure (r' ^. Wr.responseBody . #artists)
        (ctx $>) <$> pure r
      SearchRecordings search -> do
        let qterms =
              catMaybes
                [ fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #artist,
                  fmap (\a -> "release:\"" <> a <> "\"") $ search ^. #album,
                  fmap (\a -> "recording:\"" <> a <> "\"") $ search ^. #title
                ]
        case qterms of
          [] -> (ctx $>) <$> pure []
          ts -> do
            let q = T.intercalate " AND " ts
            let opts =
                  mbWreqDefaults & Wr.param "query" .~ [q]
            let url = baseUrl <> "/recording"
            r <- do
              r' :: Wr.Response RecordingSearchResult <- getWithJson opts url
              pure (r' ^. Wr.responseBody . #recordings)
            (ctx $>) <$> pure r
      GetArtist artistId -> do
        let opts = mbWreqDefaults
        let url = baseUrl <> "/artist/" <> artistId ^. #mbid
        r <- getWithJson opts url <&> view Wr.responseBody
        (ctx $>) <$> pure r
      GetArtistReleaseGroups artistId -> do
        let opts =
              mbWreqDefaults
                & Wr.param "artist" .~ [artistId ^. #mbid]
                & Wr.param "limit" .~ ["100"]
        let url = baseUrl <> "/release-group/"
        r <- getWithJson opts url <&> view Wr.responseBody
        (ctx $>) <$> pure r
      GetArtistReleases artistId -> do
        let opts =
              mbWreqDefaults
                & Wr.param "artist" .~ [artistId ^. #mbid]
                & Wr.param "limit" .~ ["100"]
        let url = baseUrl <> "/release/"
        r <- getWithJson opts url <&> view Wr.responseBody
        (ctx $>) <$> pure r
      GetArtistRecordings artistId -> do
        let opts =
              mbWreqDefaults
                & Wr.param "artist" .~ [artistId ^. #mbid]
                & Wr.param "limit" .~ ["100"]
        let url = baseUrl <> "/recording/"
        r <- getWithJson opts url <&> view Wr.responseBody
        (ctx $>) <$> pure r
  alg hdl (R other) ctx =
    MusicBrainzServiceIOC $
      alg (runMusicBrainzServiceIOC . hdl) (R (R other)) ctx

mbWreqDefaults :: Wr.Options
mbWreqDefaults =
  Wr.defaults
    & Wr.header "User-Agent" .~ [meloUserAgent]
    & Wr.header "Accept" .~ ["application/json"]
    & Wr.param "fmt" .~ ["json"]

baseUrl :: IsString s => s
baseUrl = "http://musicbrainz.org/ws/2"

runMusicBrainzServiceIO ::
  ( Has Logging sig m,
    Has (Lift IO) sig m,
    Has Empty sig m
  ) =>
  WrS.Session ->
  MusicBrainzServiceIOC m a ->
  m a
runMusicBrainzServiceIO sess =
  runRateLimitIO mbRateLimitConfig
    . runHttpEmpty
    . runHttpSession sess
    . runMusicBrainzServiceIOC

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
