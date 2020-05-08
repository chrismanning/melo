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
    getRelease,
    searchReleases,
    getArtistReleases,
    Release (..),
    ReleaseSearch (..),
    getReleaseGroup,
    searchReleaseGroups,
    getArtistReleaseGroups,
    ReleaseGroup (..),
    artistIdTag,
    originalArtistIdTag,
    albumArtistIdTag,
    trackIdTag,
    releaseTrackIdTag,
    releaseGroupIdTag,
    releaseIdTag,
    getArtistFromMetadata,
    getReleaseFromMetadata,
  )
where

import Control.Algebra
import Control.Carrier.Cull.Church
import Control.Carrier.Empty.Church as E
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.TokenLimiter
import Control.Lens hiding (from, lens)
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.ByteString.Char8 (ByteString)
import Data.Default
import Data.Functor
import Data.Generics.Labels ()
import Data.Hashable
import Data.Kind (Type)
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Melo.Common.Cache
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
  deriving stock (Generic)
  deriving anyclass (Hashable)

newtype ArtistSearchResult = ArtistSearchResult
  { artists :: Maybe [Artist]
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Artist = Artist
  { id :: MusicBrainzId,
    name :: Text,
    disambiguation :: Maybe Text,
    sortName :: Maybe Text,
    country :: Maybe Text,
    score :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON Artist where
  parseJSON = genericParseJSON mbAesonOptions

data ReleaseSearch = ReleaseSearch
  { albumArtist :: Maybe Text,
    albumTitle :: Maybe Text
  }
  deriving (Generic, Default, Hashable)

newtype ReleaseSearchResult = ReleaseSearchResult
  { releases :: Maybe [Release]
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Release = Release
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe [ArtistCredit],
    date :: Maybe Text,
    score :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON Release where
  parseJSON = genericParseJSON mbAesonOptions

data ArtistCredit = ArtistCredit
  { name :: Maybe Text,
    artist :: Artist
  }
  deriving (Show, Generic)

instance FromJSON ArtistCredit where
  parseJSON = genericParseJSON mbAesonOptions

newtype ReleaseGroupSearchResult = ReleaseGroupSearchResult
  { releaseGroups :: Maybe [ReleaseGroup]
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data ReleaseGroup = ReleaseGroup
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe [ArtistCredit]
  }
  deriving (Show, Generic)

instance FromJSON ReleaseGroup where
  parseJSON = genericParseJSON mbAesonOptions

data RecordingSearch = RecordingSearch
  { title :: Maybe Text,
    artist :: Maybe Text,
    album :: Maybe Text
  }
  deriving (Show, Generic, Hashable)

newtype RecordingSearchResult = RecordingSearchResult
  { recordings :: Maybe [Recording]
  }
  deriving (Show, Generic)
  deriving anyclass (FromJSON)

data Recording = Recording
  { id :: MusicBrainzId,
    score :: Int,
    title :: Text,
    artistCredit :: Maybe [ArtistCredit],
    releases :: [Release]
  }
  deriving (Show, Generic)

instance FromJSON Recording where
  parseJSON = genericParseJSON mbAesonOptions

data MusicBrainzService (m :: Type -> Type) a where
  SearchReleases :: ReleaseSearch -> MusicBrainzService m [Release]
  SearchReleaseGroups :: ReleaseSearch -> MusicBrainzService m [ReleaseGroup]
  SearchArtists :: ArtistSearch -> MusicBrainzService m [Artist]
  SearchRecordings :: RecordingSearch -> MusicBrainzService m [Recording]
  GetArtist :: MusicBrainzId -> MusicBrainzService m (Maybe Artist)
  GetArtistReleaseGroups :: MusicBrainzId -> MusicBrainzService m [ReleaseGroup]
  GetArtistReleases :: MusicBrainzId -> MusicBrainzService m [Release]
  GetArtistRecordings :: MusicBrainzId -> MusicBrainzService m [Recording]
  GetRelease :: MusicBrainzId -> MusicBrainzService m (Maybe Release)
  GetReleaseGroup :: MusicBrainzId -> MusicBrainzService m (Maybe ReleaseGroup)

instance Hashable (MusicBrainzService m a) where
  hashWithSalt s (SearchReleases r) = hashWithSalt s ("MusicBrainzService.SearchReleases" :: ByteString) + hashWithSalt s r
  hashWithSalt s (SearchReleaseGroups r) = hashWithSalt s ("MusicBrainzService.SearchReleaseGroups" :: ByteString) + hashWithSalt s r
  hashWithSalt s (SearchArtists a) = hashWithSalt s ("MusicBrainzService.SearchArtists" :: ByteString) + hashWithSalt s a
  hashWithSalt s (SearchRecordings r) = hashWithSalt s ("MusicBrainzService.SearchRecordings" :: ByteString) + hashWithSalt s r
  hashWithSalt s (GetArtist mbid) = hashWithSalt s ("MusicBrainzService.GetArtist" :: ByteString) + hashWithSalt s mbid
  hashWithSalt s (GetArtistReleaseGroups mbid) = hashWithSalt s ("MusicBrainzService.GetArtistReleaseGroups" :: ByteString) + hashWithSalt s mbid
  hashWithSalt s (GetArtistReleases mbid) = hashWithSalt s ("MusicBrainzService.GetArtistReleases" :: ByteString) + hashWithSalt s mbid
  hashWithSalt s (GetArtistRecordings mbid) = hashWithSalt s ("MusicBrainzService.GetArtistRecordings" :: ByteString) + hashWithSalt s mbid
  hashWithSalt s (GetRelease mbid) = hashWithSalt s ("MusicBrainzService.GetRelease" :: ByteString) + hashWithSalt s mbid
  hashWithSalt s (GetReleaseGroup mbid) = hashWithSalt s ("MusicBrainzService.GetReleaseGroup" :: ByteString) + hashWithSalt s mbid

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

getRelease :: Has MusicBrainzService sig m => MusicBrainzId -> m (Maybe Release)
getRelease mbid = send (GetRelease mbid)

getReleaseGroup :: Has MusicBrainzService sig m => MusicBrainzId -> m (Maybe ReleaseGroup)
getReleaseGroup mbid = send (GetReleaseGroup mbid)

getArtistFromMetadata ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m
  ) =>
  Metadata ->
  m [Artist]
getArtistFromMetadata m = do
  let tag = lens m
  let ts = m ^. #tags
  runCullM P.id $ cull $
    getArtistByMusicBrainzId (V.toList $ ts ^. tag artistIdTag)
      <|> getArtistByAlbum (ts ^. tag M.album ^? _head) (ts ^. tag M.albumArtist ^? _head)
      <|> getArtistByTrackArtistName (ts ^. tag M.artist ^? _head)

getArtistByMusicBrainzId ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  [Text] ->
  m [Artist]
getArtistByMusicBrainzId [] = E.empty
getArtistByMusicBrainzId artistIds = do
  artists <- catMaybes <$> forM artistIds (getArtist . MusicBrainzId)
  $(logDebugShow) artists
  pure artists

getArtistByAlbum ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  Maybe Text ->
  Maybe Text ->
  m [Artist]
getArtistByAlbum Nothing Nothing = E.empty
getArtistByAlbum albumTitle albumArtist = do
  release <- getReleaseByAlbum albumTitle albumArtist
  let artists = release ^.. #artistCredit . traverse . traverse . #artist . #id . coerced
  getArtistByMusicBrainzId artists

getArtistByTrackArtistName ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  Maybe Text ->
  m [Artist]
getArtistByTrackArtistName Nothing = E.empty
getArtistByTrackArtistName (Just trackArtist) = do
  artists <- searchArtists ArtistSearch {artist = trackArtist}
  $(logDebugShow) artists
  case artists of
    [] -> E.empty
    _ -> pure artists

getReleaseFromMetadata ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m
  ) =>
  Metadata ->
  m (Maybe Release)
getReleaseFromMetadata m = do
  let tag = lens m
  let ts = m ^. #tags
  runCullA $ cull $
    getReleaseByMusicBrainzId (V.toList $ ts ^. tag releaseIdTag)
      <|> getReleaseByAlbum (ts ^. tag M.album ^? _head) (ts ^. tag M.albumArtist ^? _head)

getReleaseByMusicBrainzId ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  [Text] ->
  m Release
getReleaseByMusicBrainzId [] = E.empty
getReleaseByMusicBrainzId releaseIds = do
  releases <- catMaybes <$> forM releaseIds (getRelease . MusicBrainzId)
  $(logDebugShow) releases
  case releases of
    [release] -> pure release
    _ -> E.empty

getReleaseByAlbum ::
  ( Has MusicBrainzService sig m,
    Has Logging sig m,
    Has NonDet sig m
  ) =>
  Maybe Text ->
  Maybe Text ->
  m Release
getReleaseByAlbum Nothing Nothing = E.empty
getReleaseByAlbum albumTitle albumArtist = do
  releases <- searchReleases def {albumArtist = albumArtist, albumTitle = albumTitle}
  $(logDebugShow) releases
  let releases' = filter (\release -> release ^. #score == Just 100) releases
  case releases' of
    [release] -> pure release
    _ -> E.empty

newtype MusicBrainzServiceIOC m a = MusicBrainzServiceIOC
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
          [] -> pure $ ctx $> []
          ts -> do
            let q = T.intercalate " AND " ts
            let opts = mbWreqDefaults & Wr.param "query" .~ [q]
            let url = baseUrl <> "/release"
            r <- do
              r' :: Wr.Response ReleaseSearchResult <- getWithJson opts url
              pure (r' ^. Wr.responseBody . #releases)
            pure $ ctx $> fromMaybe [] r
      SearchReleaseGroups search -> do
        let qterms =
              catMaybes
                [ fmap (\t -> "release:\"" <> t <> "\"") $ search ^. #albumTitle,
                  fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #albumArtist
                ]
        case qterms of
          [] -> pure $ ctx $> []
          ts -> do
            let q = T.intercalate " AND " ts
            let opts = mbWreqDefaults & Wr.param "query" .~ [q]
            let url = baseUrl <> "/release-group"
            r <- do
              r' :: Wr.Response ReleaseGroupSearchResult <- getWithJson opts url
              pure (r' ^. Wr.responseBody . #releaseGroups)
            pure $ ctx $> fromMaybe [] r
      SearchArtists search -> do
        let opts =
              mbWreqDefaults
                & Wr.param "query" .~ ["artist:\"" <> search ^. #artist <> "\""]
        let url = baseUrl <> "/artist"
        r <- do
          r' :: Wr.Response ArtistSearchResult <- getWithJson opts url
          pure (r' ^. Wr.responseBody . #artists)
        pure $ ctx $> fromMaybe [] r
      SearchRecordings search -> do
        let qterms =
              catMaybes
                [ fmap (\a -> "artist:\"" <> a <> "\"") $ search ^. #artist,
                  fmap (\a -> "release:\"" <> a <> "\"") $ search ^. #album,
                  fmap (\a -> "recording:\"" <> a <> "\"") $ search ^. #title
                ]
        case qterms of
          [] -> pure $ ctx $> []
          ts -> do
            let q = T.intercalate " AND " ts
            let opts =
                  mbWreqDefaults & Wr.param "query" .~ [q]
            let url = baseUrl <> "/recording"
            r <- do
              r' :: Wr.Response RecordingSearchResult <- getWithJson opts url
              pure (r' ^. Wr.responseBody . #recordings)
            pure $ ctx $> fromMaybe [] r
      GetArtist artistId -> do
        let opts = mbWreqDefaults
        let url = baseUrl <> "/artist/" <> artistId ^. coerced
        r <- getWithJson opts url <&> view Wr.responseBody
        pure $ ctx $> r
      GetArtistReleaseGroups artistId -> do
        let opts =
              mbWreqDefaults
                & Wr.param "artist" .~ [artistId ^. coerced]
                & Wr.param "limit" .~ ["100"]
        let url = baseUrl <> "/release-group/"
        r <- getWithJson opts url <&> view Wr.responseBody
        pure $ ctx $> r
      GetArtistReleases artistId -> do
        let opts =
              mbWreqDefaults
                & Wr.param "artist" .~ [artistId ^. coerced]
                & Wr.param "limit" .~ ["100"]
        let url = baseUrl <> "/release/"
        r <- getWithJson opts url <&> view Wr.responseBody
        pure $ ctx $> r
      GetArtistRecordings artistId -> do
        let opts =
              mbWreqDefaults
                & Wr.param "artist" .~ [artistId ^. coerced]
                & Wr.param "limit" .~ ["100"]
        let url = baseUrl <> "/recording/"
        r <- getWithJson opts url <&> view Wr.responseBody
        pure $ ctx $> r
      GetRelease releaseId -> do
        let opts = mbWreqDefaults
        let url = baseUrl <> "/release/" <> releaseId ^. coerced
        r <- getWithJson opts url <&> view Wr.responseBody
        pure $ ctx $> r
      GetReleaseGroup releaseGroupId -> do
        let opts = mbWreqDefaults
        let url = baseUrl <> "/release-group/" <> releaseGroupId ^. coerced
        r <- getWithJson opts url <&> view Wr.responseBody
        pure $ ctx $> r
  alg hdl (R other) ctx =
    MusicBrainzServiceIOC $
      alg (runMusicBrainzServiceIOC . hdl) (R (R other)) ctx

--newtype CachedMusicBrainzServiceIOC m a = CachedMusicBrainzServiceIOC
--  { runCachedMusicBrainzServiceIOC :: HttpSessionIOC (RateLimitIOC m) a
--  }
--  deriving newtype (Applicative, Functor, Monad)
--
--instance Algebra (Cache :+: MusicBrainzService :+: sig) (CachedMusicBrainzServiceIOC m) where
--  alg hdl sig ctx = undefined

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

releaseIdTag :: TagMapping
releaseIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        id3v2_3 = caseInsensitiveMapping "TXXX:MusicBrainz Album Id",
        id3v2_4 = caseInsensitiveMapping "TXXX:MusicBrainz Album Id"
      }
