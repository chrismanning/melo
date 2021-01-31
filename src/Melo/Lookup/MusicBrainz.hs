{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Lookup.MusicBrainz
  ( MusicBrainzId (..),
    runMusicBrainzServiceIO,
    MusicBrainzService (..),
    MusicBrainzServiceT (..),
    Artist (..),
    ArtistCredit (..),
    ArtistSearch (..),
    Recording (..),
    RecordingSearch (..),
    Release (..),
    ReleaseSearch (..),
    ReleaseGroup (..),
    artistIdTag,
    originalArtistIdTag,
    albumArtistIdTag,
    albumIdTag,
    trackIdTag,
    releaseTrackIdTag,
    releaseGroupIdTag,
    releaseIdTag,
    getArtistFromMetadata,
    getReleaseFromMetadata,
  )
where

import Control.Applicative as A
import Control.Concurrent.TokenLimiter
import Control.Lens hiding (from, lens)
import Control.Monad
import Control.Monad.Reader
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.Default
import Data.Generics.Labels ()
import Data.Hashable
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Common.RateLimit
import Melo.Format.Mapping
  ( FieldMappings (..),
    TagMapping (..),
    caseInsensitiveMapping,
    singletonTagMapping,
  )
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import qualified Network.Wreq as Wr
import qualified Network.Wreq.Session as WrS
import Prelude as P

newtype MusicBrainzId = MusicBrainzId
  { mbid :: Text
  }
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, Hashable)

newtype ArtistSearch = ArtistSearch
  { artist :: Text
  }
  deriving stock (Show, Generic, Eq)

instance Hashable ArtistSearch

newtype ArtistSearchResult = ArtistSearchResult
  { artists :: Maybe [Artist]
  }
  deriving (Show, Generic, Eq)

instance Hashable ArtistSearchResult

instance FromJSON ArtistSearchResult

data Artist = Artist
  { id :: MusicBrainzId,
    name :: Text,
    disambiguation :: Maybe Text,
    sortName :: Maybe Text,
    country :: Maybe Text,
    score :: Maybe Int
  }
  deriving (Show, Generic, Eq)

instance Hashable Artist

instance FromJSON Artist where
  parseJSON = genericParseJSON mbAesonOptions

data ReleaseSearch = ReleaseSearch
  { albumArtist :: Maybe Text,
    albumTitle :: Maybe Text
  }
  deriving (Show, Generic, Eq)

instance Hashable ReleaseSearch

instance Default ReleaseSearch

newtype ReleaseSearchResult = ReleaseSearchResult
  { releases :: Maybe [Release]
  }
  deriving (Show, Generic, Eq)

instance FromJSON ReleaseSearchResult

data Release = Release
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe [ArtistCredit],
    date :: Maybe Text,
    score :: Maybe Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON Release where
  parseJSON = genericParseJSON mbAesonOptions

data ArtistCredit = ArtistCredit
  { name :: Maybe Text,
    artist :: Artist
  }
  deriving (Show, Generic, Eq)

instance FromJSON ArtistCredit where
  parseJSON = genericParseJSON mbAesonOptions

newtype ReleaseGroupSearchResult = ReleaseGroupSearchResult
  { releaseGroups :: Maybe [ReleaseGroup]
  }
  deriving (Show, Generic, Eq)
  deriving newtype (FromJSON)

data ReleaseGroup = ReleaseGroup
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe [ArtistCredit]
  }
  deriving (Show, Generic, Eq)

instance FromJSON ReleaseGroup where
  parseJSON = genericParseJSON mbAesonOptions

data RecordingSearch = RecordingSearch
  { title :: Maybe Text,
    artist :: Maybe Text,
    album :: Maybe Text
  }
  deriving (Show, Generic, Eq)

instance Hashable RecordingSearch

newtype RecordingSearchResult = RecordingSearchResult
  { recordings :: Maybe [Recording]
  }
  deriving (Show, Generic, Eq)

instance FromJSON RecordingSearchResult

data Recording = Recording
  { id :: MusicBrainzId,
    score :: Int,
    title :: Text,
    artistCredit :: Maybe [ArtistCredit],
    releases :: [Release]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Recording where
  parseJSON = genericParseJSON mbAesonOptions

class Monad m => MusicBrainzService m where
  searchReleases :: ReleaseSearch -> m [Release]
  searchReleaseGroups :: ReleaseSearch -> m [ReleaseGroup]
  searchArtists :: ArtistSearch -> m [Artist]
  searchRecordings :: RecordingSearch -> m [Recording]
  getArtist :: MusicBrainzId -> m (Maybe Artist)
  getArtistReleaseGroups :: MusicBrainzId -> m [ReleaseGroup]
  getArtistReleases :: MusicBrainzId -> m [Release]
  getArtistRecordings :: MusicBrainzId -> m [Recording]
  getRelease :: MusicBrainzId -> m (Maybe Release)
  getReleaseGroup :: MusicBrainzId -> m (Maybe ReleaseGroup)

getArtistFromMetadata ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  Metadata ->
  m [Artist]
getArtistFromMetadata Metadata {tags, lens} =
  getArtistByMusicBrainzId (V.toList $ tags ^. lens artistIdTag)
    `orM` getArtistByAlbum (tags ^? lens M.album . _head) (tags ^? lens M.albumArtist . _head)
    `orM` getArtistByTrackArtistName (tags ^? lens M.artist . _head)

getArtistByMusicBrainzId ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  [Text] ->
  m [Artist]
getArtistByMusicBrainzId [] = pure A.empty
getArtistByMusicBrainzId artistIds = do
  artists <- catMaybes <$> forM artistIds (getArtist . MusicBrainzId)
  $(logDebugShow) artists
  pure artists

getArtistByAlbum ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  Maybe Text ->
  Maybe Text ->
  m [Artist]
getArtistByAlbum Nothing Nothing = pure A.empty
getArtistByAlbum albumTitle albumArtist = do
  release <- getReleaseByAlbum albumTitle albumArtist
  let artists = release ^.. _Just . #artistCredit . traverse . traverse . #artist . #id . coerced
  getArtistByMusicBrainzId artists

getArtistByTrackArtistName ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  Maybe Text ->
  m [Artist]
getArtistByTrackArtistName Nothing = pure A.empty
getArtistByTrackArtistName (Just trackArtist) = do
  artists <- searchArtists ArtistSearch {artist = trackArtist}
  $(logDebugShow) artists
  case artists of
    [] -> pure A.empty
    artists' -> pure artists'

orM :: (Monad m, Alternative f, Eq (f a)) => m (f a) -> m (f a) -> m (f a)
orM a b =
  a >>= \case
    a'
      | a' == A.empty ->
        b >>= \case
          b' | b' == A.empty -> pure A.empty
          b' -> pure b'
    a' -> pure a'

getReleaseFromMetadata ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  Metadata ->
  m (Maybe Release)
getReleaseFromMetadata Metadata {tags, lens} =
  getReleaseByMusicBrainzId (V.toList $ tags ^. lens releaseIdTag)
    `orM` getReleaseByAlbum (tags ^? lens M.album . _head) (tags ^? lens M.albumArtist . _head)

getReleaseByMusicBrainzId ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  [Text] ->
  m (Maybe Release)
getReleaseByMusicBrainzId [] = pure A.empty
getReleaseByMusicBrainzId releaseIds = do
  releases <- catMaybes <$> forM releaseIds (getRelease . MusicBrainzId)
  $(logDebugShow) releases
  case releases of
    [release] -> pure $ Just release
    _noMatchingRelease -> pure A.empty

getReleaseByAlbum ::
  ( MusicBrainzService m,
    Logging m
  ) =>
  Maybe Text ->
  Maybe Text ->
  m (Maybe Release)
getReleaseByAlbum Nothing Nothing = pure A.empty
getReleaseByAlbum albumTitle albumArtist = do
  releases <- searchReleases def {albumArtist = albumArtist, albumTitle = albumTitle}
  $(logDebugShow) releases
  let releases' = filter (\release -> release ^. #score == Just 100) releases
  case releases' of
    [release] -> pure $ Just release
    _noMatchingRelease -> pure A.empty

newtype MusicBrainzServiceT m a = MusicBrainzServiceT
  { runMusicBrainzServiceT :: HttpSessionT (RateLimitT m) a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance MonadTrans MusicBrainzServiceT where
  lift m = MusicBrainzServiceT $ HttpSessionT $ ReaderT $ const $ RateLimitT $ ReaderT $ const m

instance
  ( MonadIO m,
    Logging m
  ) =>
  MusicBrainzService (MusicBrainzServiceT m)
  where
  searchReleases search = MusicBrainzServiceT $ do
    waitReady
    let qterms =
          catMaybes
            [ fmap (\t -> "release:\"" <> t <> "\"") $ search ^. #albumTitle,
              fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #albumArtist
            ]
    case qterms of
      [] -> pure []
      ts -> do
        let q = T.intercalate " AND " ts
        let opts = mbWreqDefaults & Wr.param "query" .~ [q]
        let url = baseUrl <> "/release"
        getWithJson @_ @ReleaseSearchResult opts url >>= \case
          Left e -> do
            $(logError) $ "error searching for matching releases: " <> show e
            pure []
          Right r -> pure $ fromMaybe [] $ r ^. Wr.responseBody . #releases
  searchReleaseGroups search = MusicBrainzServiceT $ do
    waitReady
    let qterms =
          catMaybes
            [ fmap (\t -> "release:\"" <> t <> "\"") $ search ^. #albumTitle,
              fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #albumArtist
            ]
    case qterms of
      [] -> pure []
      ts -> do
        let q = T.intercalate " AND " ts
        let opts = mbWreqDefaults & Wr.param "query" .~ [q]
        let url = baseUrl <> "/release-group"
        getWithJson @_ @ReleaseGroupSearchResult opts url >>= \case
          Left e -> do
            $(logError) $ "error searching for matching release groups: " <> show e
            pure []
          Right r -> pure $ fromMaybe [] $ r ^. Wr.responseBody . #releaseGroups
  searchArtists search = MusicBrainzServiceT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "query" .~ ["artist:\"" <> search ^. #artist <> "\""]
    let url = baseUrl <> "/artist"
    getWithJson @_ @ArtistSearchResult opts url >>= \case
      Left e -> do
        $(logError) $ "error searching for matching artists: " <> show e
        pure []
      Right r -> pure $ fromMaybe [] $ r ^. Wr.responseBody . #artists
  searchRecordings search = MusicBrainzServiceT $ do
    waitReady
    let qterms =
          catMaybes
            [ fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #artist,
              fmap (\a -> "release:\"" <> a <> "\"") $ search ^. #album,
              fmap (\a -> "recording:\"" <> a <> "\"") $ search ^. #title
            ]
    case qterms of
      [] -> pure []
      ts -> do
        let q = T.intercalate " AND " ts
        let opts =
              mbWreqDefaults & Wr.param "query" .~ [q]
        let url = baseUrl <> "/recording"
        getWithJson @_ @RecordingSearchResult opts url >>= \case
          Left e -> do
            $(logError) $ "error searching for matching recordings: " <> show e
            pure []
          Right r -> pure $ fromMaybe [] $ r ^. Wr.responseBody . #recordings
  getArtist artistId = MusicBrainzServiceT $ do
    waitReady
    let opts = mbWreqDefaults
    let url = baseUrl <> "/artist/" <> artistId ^. coerced
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting artist " <> show artistId <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody
  getArtistReleaseGroups artistId = MusicBrainzServiceT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "artist" .~ [artistId ^. coerced]
            & Wr.param "limit" .~ ["100"]
    let url = baseUrl <> "/release-group/"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting release groups for artist " <> show artistId <> ": " <> show e
        pure []
      Right r -> pure $ r ^. Wr.responseBody
  getArtistReleases artistId = MusicBrainzServiceT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "artist" .~ [artistId ^. coerced]
            & Wr.param "limit" .~ ["100"]
    let url = baseUrl <> "/release/"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting releases for artist " <> show artistId <> ": " <> show e
        pure []
      Right r -> pure $ r ^. Wr.responseBody
  getArtistRecordings artistId = MusicBrainzServiceT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "artist" .~ [artistId ^. coerced]
            & Wr.param "limit" .~ ["100"]
    let url = baseUrl <> "/recording/"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting recordings for artist " <> show artistId <> ": " <> show e
        pure []
      Right r -> pure $ r ^. Wr.responseBody
  getRelease releaseId = MusicBrainzServiceT $ do
    waitReady
    let opts = mbWreqDefaults
    let url = baseUrl <> "/release/" <> releaseId ^. coerced
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting release " <> show releaseId <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody
  getReleaseGroup releaseGroupId = MusicBrainzServiceT $ do
    waitReady
    let opts = mbWreqDefaults
    let url = baseUrl <> "/release-group/" <> releaseGroupId ^. coerced
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting release group " <> show releaseGroupId <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody

--newtype CachedMusicBrainzServiceIOC m a = CachedMusicBrainzServiceIOC
--  { runCachedMusicBrainzServiceIOC :: HttpSessionIOC (RateLimitIOC m) a
--  }
--  deriving newtype (Applicative, Functor, Monad)
--
--instance Algebra (Cache :+: MusicBrainzService :+: sig) (CachedMusicBrainzServiceIOC m) where
--  alg hdl sig ctx = error "unimplemented"

mbWreqDefaults :: Wr.Options
mbWreqDefaults =
  Wr.defaults
    & Wr.header "User-Agent" .~ [meloUserAgent]
    & Wr.header "Accept" .~ ["application/json"]
    & Wr.param "fmt" .~ ["json"]

baseUrl :: IsString s => s
baseUrl = "http://musicbrainz.org/ws/2"

runMusicBrainzServiceIO ::
  ( MonadIO m
  ) =>
  WrS.Session ->
  MusicBrainzServiceT m a ->
  m a
runMusicBrainzServiceIO sess =
  runRateLimitIO mbRateLimitConfig
    . runHttpSession sess
    . runMusicBrainzServiceT

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

albumIdTag :: TagMapping
albumIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Album Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Album Id"
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

releaseIdTag :: TagMapping
releaseIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Album Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Album Id"
      }
