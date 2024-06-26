{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Lookup.MusicBrainz
  ( MusicBrainzId (..),
    MusicBrainzService (..),
    Artist (..),
    ArtistAlias (..),
    Area (..),
    CountrySubdivision (..),
    ArtistCredit (..),
    ArtistSearch (..),
    Recording (..),
    RecordingSearch (..),
    Release (..),
    ReleaseSearch (..),
    ReleaseGroup (..),
    LabelInfo (..),
    Label (..),
    artistIdTag,
    originalArtistIdTag,
    albumArtistIdTag,
    albumIdTag,
    trackIdTag,
    recordingIdTag,
    releaseTrackIdTag,
    releaseGroupIdTag,
    releaseIdTag,
    getReleaseOrGroup,
    getReleaseAndGroup,
    getReleaseFromMetadata,
    getReleaseGroupFromMetadata,
    getRecordingFromMetadata,
    truncateDate,
    (<<|>>),
    initMusicBrainzConfig,
  )
where

import Control.Concurrent.Classy
import Control.Concurrent.STM.Map as SM
import Control.Concurrent.TokenLimiter
import Control.Monad.Reader
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.Aeson.Types
import Data.ByteString.Char8 qualified as C8
import Data.Char
import Data.Default
import Data.Generics.Labels ()
import Data.Hashable
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Vector qualified as V
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.Http as Http
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Format.Mapping
  ( FieldMappings (..),
    TagMapping (..),
    caseInsensitiveMapping,
    singletonTagMapping,
  )
import Melo.Format.Mapping qualified as M
import Melo.Format.Metadata qualified as F
import Network.HTTP.Client as Http
import Network.HTTP.Types.URI

newtype MusicBrainzId = MusicBrainzId
  { mbid :: Text
  }
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, Hashable)
  deriving (TextShow) via FromStringShow MusicBrainzId

newtype ArtistSearch = ArtistSearch
  { artist :: Text
  }
  deriving stock (Show, Generic, Eq, Ord)
  deriving newtype (Hashable)
  deriving (TextShow) via FromGeneric ArtistSearch

newtype ArtistSearchResult = ArtistSearchResult
  { artists :: Maybe (Vector Artist)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric ArtistSearchResult

instance FromJSON ArtistSearchResult

data Artist = Artist
  { id :: MusicBrainzId,
    name :: Text,
    disambiguation :: Maybe Text,
    sortName :: Maybe Text,
    country :: Maybe Text,
    score :: Maybe Int,
    area :: Maybe Area,
    beginArea :: Maybe Area,
    aliases :: Maybe (Vector ArtistAlias)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric Artist

instance Ord Artist where
  a <= b = a.id <= b.id && a.name <= b.name

instance FromJSON Artist where
  parseJSON = genericParseJSON mbAesonOptions

instance ToJSON Artist where
  toJSON = genericToJSON mbAesonOptions
  toEncoding = genericToEncoding mbAesonOptions

data ArtistAlias = ArtistAlias
  { name :: Text,
    sortName :: Maybe Text,
    type' :: Maybe Text
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric ArtistAlias

instance FromJSON ArtistAlias where
  parseJSON = withObject "ArtistAlias" $ \v ->
    ArtistAlias
      <$> v .: "name"
      <*> v .:? "sort-name"
      <*> v .:? "type"

instance ToJSON ArtistAlias where
  toJSON = genericToJSON mbAesonOptions
  toEncoding = genericToEncoding mbAesonOptions

data Area = Area
  { iso3166_2codes :: Maybe (Vector CountrySubdivision),
    iso3166_1codes :: Maybe (Vector Text)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric Area

instance FromJSON Area where
  parseJSON = withObject "Area" $ \v ->
    Area
      <$> v .:? "iso-3166-2-codes"
      <*> v .:? "iso-3166-1-codes"

instance ToJSON Area where
  toJSON Area {..} = A.object ["iso-3166-2-codes" A..= iso3166_2codes, "iso-3166-1-codes" A..= iso3166_1codes]
  toEncoding Area {..} = A.pairs ("iso-3166-2-codes" A..= iso3166_2codes <> "iso-3166-1-codes" A..= iso3166_1codes)

data CountrySubdivision = CountrySubdivision
  { country :: Text,
    subdivision :: Text
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric CountrySubdivision

instance FromJSON CountrySubdivision where
  parseJSON (String s) = case T.split (== '-') s of
    [c, s] | T.length c == 2 -> pure $ CountrySubdivision c s
    _ ->
      prependFailure
        "parsing CountrySubdivision failed, "
        (fail "expected ISO-3166-2 format")
  parseJSON invalid =
    prependFailure
      "parsing CountrySubdivision failed, "
      (typeMismatch "String" invalid)

instance ToJSON CountrySubdivision where
  toJSON = genericToJSON mbAesonOptions
  toEncoding = genericToEncoding mbAesonOptions

data ReleaseSearch = ReleaseSearch
  { albumArtists :: Maybe [Text],
    albumTitle :: Maybe Text,
    date :: Maybe Text,
    catNum :: Maybe Text
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (TextShow) via FromGeneric ReleaseSearch

instance Default ReleaseSearch

instance Hashable ReleaseSearch

newtype ReleaseSearchResult = ReleaseSearchResult
  { releases :: Maybe (Vector Release)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric ReleaseSearchResult

instance FromJSON ReleaseSearchResult

data Release = Release
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe (Vector ArtistCredit),
    date :: Maybe Text,
    score :: Maybe Int,
    primaryType :: Maybe Text,
    labelInfo :: Maybe (Vector LabelInfo)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric Release

instance FromJSON Release where
  parseJSON = genericParseJSON mbAesonOptions

data ArtistCredit = ArtistCredit
  { name :: Maybe Text,
    artist :: Artist
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (TextShow) via FromGeneric ArtistCredit

instance FromJSON ArtistCredit where
  parseJSON = genericParseJSON mbAesonOptions

instance ToJSON ArtistCredit where
  toJSON = genericToJSON mbAesonOptions
  toEncoding = genericToEncoding mbAesonOptions

data LabelInfo = LabelInfo
  { catalogNumber :: Maybe Text,
    label :: Maybe Label
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric LabelInfo

instance FromJSON LabelInfo where
  parseJSON = genericParseJSON mbAesonOptions

data Label = Label
  { id :: MusicBrainzId,
    name :: Text
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric Label

instance FromJSON Label where
  parseJSON = genericParseJSON mbAesonOptions

newtype ReleaseGroupSearchResult = ReleaseGroupSearchResult
  { releaseGroups :: Maybe (Vector ReleaseGroup)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric ReleaseGroupSearchResult

instance FromJSON ReleaseGroupSearchResult where
  parseJSON = genericParseJSON mbAesonOptions

data ReleaseGroup = ReleaseGroup
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe (Vector ArtistCredit),
    firstReleaseDate :: Maybe Text,
    primaryType :: Maybe Text,
    score :: Maybe Int
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric ReleaseGroup

instance FromJSON ReleaseGroup where
  parseJSON = genericParseJSON mbAesonOptions

data RecordingSearch = RecordingSearch
  { title :: Maybe Text,
    trackNumber :: Maybe Text,
    artists :: Maybe [Text],
    album :: Maybe Text,
    releaseId :: Maybe Text,
    releaseGroupId :: Maybe Text,
    isrc :: Maybe Text
  }
  deriving (Show, Generic, Eq, Ord)
  deriving (TextShow) via FromGeneric RecordingSearch

instance Default RecordingSearch

instance Hashable RecordingSearch

newtype RecordingSearchResult = RecordingSearchResult
  { recordings :: Maybe (Vector Recording)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric RecordingSearchResult

instance FromJSON RecordingSearchResult

data Recording = Recording
  { id :: MusicBrainzId,
    score :: Maybe Int,
    title :: Text,
    artistCredit :: Maybe (Vector ArtistCredit),
    releases :: Maybe (Vector Release)
  }
  deriving (Show, Generic, Eq)
  deriving (TextShow) via FromGeneric Recording

instance FromJSON Recording where
  parseJSON = genericParseJSON mbAesonOptions

class (Monad m) => MusicBrainzService m where
  searchReleases :: ReleaseSearch -> m (Vector Release)
  searchReleaseGroups :: ReleaseSearch -> m (Vector ReleaseGroup)
  searchArtists :: ArtistSearch -> m (Vector Artist)
  searchRecordings :: RecordingSearch -> m (Vector Recording)
  getArtist :: MusicBrainzId -> m (Maybe Artist)
  getArtistReleaseGroups :: MusicBrainzId -> m (Vector ReleaseGroup)
  getArtistReleases :: MusicBrainzId -> m (Vector Release)
  getArtistRecordings :: MusicBrainzId -> m (Vector Recording)
  getRelease :: MusicBrainzId -> m (Maybe Release)
  getReleaseGroup :: MusicBrainzId -> m (Maybe ReleaseGroup)
  getRecording :: MusicBrainzId -> m (Maybe Recording)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MusicBrainzService m
  ) =>
  MusicBrainzService (t m)
  where
  searchReleases = lift . searchReleases
  searchReleaseGroups = lift . searchReleaseGroups
  searchArtists = lift . searchArtists
  searchRecordings = lift . searchRecordings
  getArtist = lift . getArtist
  getArtistReleaseGroups = lift . getArtistReleaseGroups
  getArtistReleases = lift . getArtistReleases
  getArtistRecordings = lift . getArtistRecordings
  getRelease = lift . getRelease
  getReleaseGroup = lift . getReleaseGroup
  getRecording = lift . getRecording

getReleaseOrGroup ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe (Either Release ReleaseGroup))
getReleaseOrGroup m =
  getReleaseFromMetadata m >>= \case
    Just release -> pure $ Just $ Left release
    Nothing -> fmap Right <$> getReleaseGroupFromMetadata m

getReleaseAndGroup ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe ReleaseGroup, Maybe Release)
getReleaseAndGroup m = do
  releaseGroup <- getReleaseGroupFromMetadata m
  release <- getReleaseFromMetadata m
  pure (releaseGroup, release)

getReleaseFromMetadata ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Release)
getReleaseFromMetadata m =
  getReleaseByMusicBrainzId m <<|>> getReleaseByAlbum m

getReleaseByMusicBrainzId ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Release)
getReleaseByMusicBrainzId m = firstJustM getRelease (MusicBrainzId <$> m.tagHead releaseIdTag)

getReleaseByAlbum ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Release)
getReleaseByAlbum m = do
  let albumTitle = m.tagHead M.album
  let albumArtists = m.tag M.albumArtist
  let date = m.tagHead M.originalReleaseYear <&> truncateDate
  let catNum = m.tagHead M.catalogNumber
  if isn't _Nothing albumTitle && not (null albumArtists) && isn't _Nothing catNum
    then
      find perfectScore
        <$> searchReleases def {albumArtists = Just (toList albumArtists), albumTitle, date, catNum}
    else pure Nothing

getReleaseGroupFromMetadata ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe ReleaseGroup)
getReleaseGroupFromMetadata m =
  getReleaseGroupByMusicBrainzId m <<|>> getReleaseGroupByAlbum m

getReleaseGroupByMusicBrainzId ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe ReleaseGroup)
getReleaseGroupByMusicBrainzId m = firstJustM getReleaseGroup (MusicBrainzId <$> m.tagHead releaseGroupIdTag)

getReleaseGroupByAlbum ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe ReleaseGroup)
getReleaseGroupByAlbum m = do
  let albumTitle = m.tagHead M.album
  let albumArtists = m.tag M.albumArtist
  let date = m.tagHead M.originalReleaseYear <&> truncateDate
  if isn't _Nothing albumTitle && not (null albumArtists)
    then
      find perfectScore
        <$> searchReleaseGroups def {albumArtists = Just (toList albumArtists), albumTitle, date}
    else pure Nothing

getRecordingFromMetadata ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingFromMetadata m =
  getRecordingByMusicBrainzId m
    <<|>> getRecordingByReleaseIdTags m
    <<|>> getRecordingByTrack m

getRecordingByMusicBrainzId ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingByMusicBrainzId m = firstJustM getRecording (MusicBrainzId <$> m.tagHead recordingIdTag)

getRecordingByReleaseIdTags ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingByReleaseIdTags m = do
  let title = m.tagHead M.trackTitle
  let releaseId = m.tagHead releaseIdTag
  let releaseGroupId = m.tagHead releaseGroupIdTag
  if isn't _Nothing releaseId || isn't _Nothing releaseGroupId
    then
      find perfectScore
        <$> searchRecordings
          def
            { title,
              releaseId,
              releaseGroupId
            }
    else pure Nothing

getRecordingByTrack ::
  (MusicBrainzService m) =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingByTrack m = do
  let title = m.tagHead M.trackTitle
  let album = m.tagHead M.album
  let trackNumber = m.tagHead M.trackNumber
  let artists = toList $ m.tag M.artist
  let isrc = m.tagHead M.isrc
  if isn't _Nothing title && (isn't _Nothing album || not (null artists))
    then find perfectScore <$> searchRecordings def {title, album, trackNumber, isrc, artists = Just artists}
    else pure Nothing

perfectScore :: (Integral i, HasField "score" r (Maybe i)) => r -> Bool
perfectScore r = r.score == Just 100

truncateDate :: Text -> Text
truncateDate = T.takeWhile isDigit

data QueryTerm
  = QueryTerm
      { key :: Text,
        value :: Maybe Text
      }
  | MultiTerm
      { key :: Text,
        values :: Maybe [Text]
      }

renderQueryParam :: [QueryTerm] -> Maybe Text
renderQueryParam qs = case catMaybes $ fmap encodeTerm qs of
  [] -> Nothing
  ts -> Just $ T.intercalate "%20AND%20" ts
  where
    encodeTerm q@QueryTerm {} = q.value <&> \v -> q.key <> ":%22" <> decodeUtf8 (urlEncode True (encodeUtf8 v)) <> "%22"
    encodeTerm q@MultiTerm {} = q.values <&> \v -> q.key <> ":%22" <> T.intercalate " & " (decodeUtf8 . urlEncode True . encodeUtf8 <$> v) <> "%22"

newtype RateLimitWrapper = RateLimitWrapper (LimitConfig, RateLimiter)
  deriving (Typeable)

getRateLimiter ::
  ( AppDataReader m,
    ConfigService m,
    MonadIO m
  ) =>
  m (Maybe RateLimitWrapper)
getRateLimiter =
  getConfigDefault musicbrainzConfigKey >>= \case
    config
      | config == def ->
          getAppData @RateLimitWrapper >>= \case
            Nothing -> do
              rateLimiter <- liftIO $ newRateLimiter mbRateLimitConfig
              pure $ Just $ RateLimitWrapper (mbRateLimitConfig, rateLimiter)
            rl -> pure rl
    _ -> pure Nothing

waitReady ::
  ( AppDataReader m,
    ConfigService m,
    MonadIO m
  ) =>
  m ()
waitReady =
  getRateLimiter >>= \case
    Nothing -> pure ()
    Just (RateLimitWrapper (lc, rl)) -> liftIO $ waitDebit lc rl 1

mbHttp ::
  forall a m.
  ( A.FromJSON a,
    MonadCatch m,
    MonadIO m,
    AppDataReader m,
    ConfigService m,
    Tracing m
  ) =>
  String ->
  m (Maybe a)
mbHttp url = do
  waitReady
  httpManager <- Http.getManager
  config <- getConfigDefault musicbrainzConfigKey
  parseRequest (config.baseUrl <> url) >>= httpJson @a httpManager

data CacheAggregate = CacheAggregate
  { searchReleasesStore :: SM.Map ReleaseSearch (Vector Release),
    searchReleaseGroupsStore :: SM.Map ReleaseSearch (Vector ReleaseGroup),
    searchArtistsStore :: SM.Map ArtistSearch (Vector Artist),
    searchRecordingsStore :: SM.Map RecordingSearch (Vector Recording),
    getArtistStore :: SM.Map MusicBrainzId (Maybe Artist),
    getArtistReleaseGroupsStore :: SM.Map MusicBrainzId (Vector ReleaseGroup),
    getArtistReleasesStore :: SM.Map MusicBrainzId (Vector Release),
    getArtistRecordingsStore :: SM.Map MusicBrainzId (Vector Recording),
    getReleaseStore :: SM.Map MusicBrainzId (Maybe Release),
    getReleaseGroupStore :: SM.Map MusicBrainzId (Maybe ReleaseGroup),
    getRecordingStore :: SM.Map MusicBrainzId (Maybe Recording)
  }
  deriving (Generic, Typeable)

doCache :: (Hashable k) => (CacheAggregate -> SM.Map k a) -> k -> AppM IO IO a -> AppM IO IO a
doCache f k m = do
  cache <-
    getAppData @CacheAggregate >>= \case
      Just cache -> pure cache
      Nothing -> do
        a <-
          atomically
            ( CacheAggregate
                <$> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
                <*> SM.empty
            )
        putAppData a
        pure a
  let !map = f cache
  atomically (SM.lookup k map) >>= \case
    Just a -> pure a
    Nothing -> do
      a <- m
      atomically (SM.insert k a map)
      pure a

instance MusicBrainzService (AppM IO IO) where
  searchReleases search = doCache (.searchReleasesStore) search do
    let qterms =
          [ QueryTerm "releaseaccent" search.albumTitle,
            MultiTerm "artist" search.albumArtists,
            QueryTerm "catno" search.catNum
          ]
    case renderQueryParam qterms of
      Nothing -> pure V.empty
      Just q -> do
        let url = "/release?query=" <> T.unpack q <> "&fmt=json"
        mbHttp @ReleaseSearchResult url >>= \case
          Just r -> pure $ fromMaybe V.empty $ r.releases
          Nothing -> do
            $(logWarn) "no matching releases found"
            pure V.empty
  searchReleaseGroups search = doCache (.searchReleaseGroupsStore) search do
    let qterms =
          [ QueryTerm "releasegroupaccent" search.albumTitle,
            MultiTerm "artist" search.albumArtists,
            QueryTerm "firstreleasedate" search.date
          ]
    case renderQueryParam qterms of
      Nothing -> pure V.empty
      Just q -> do
        let url = "/release-group?query=" <> T.unpack q <> "&fmt=json"
        mbHttp @ReleaseGroupSearchResult url >>= \case
          Just r -> pure $ fromMaybe V.empty r.releaseGroups
          Nothing -> do
            $(logWarn) "no matching release groups found"
            pure V.empty
  searchArtists search = doCache (.searchArtistsStore) search do
    let url = "/artist?query=artist:\"" <> decodeUtf8 (urlEncode True (encodeUtf8 search.artist)) <> "\"" <> "&fmt=json"
    mbHttp @ArtistSearchResult (T.unpack url) >>= \case
      Just r -> pure $ fromMaybe V.empty $ r.artists
      Nothing -> do
        $(logError) "no matching artists found"
        pure V.empty
  searchRecordings search = doCache (.searchRecordingsStore) search do
    let qterms =
          [ QueryTerm "recording" search.title,
            QueryTerm "tnum" search.trackNumber,
            MultiTerm "artist" search.artists,
            QueryTerm "release" search.album,
            QueryTerm "reid" search.releaseId,
            QueryTerm "rgid" search.releaseGroupId,
            QueryTerm "isrc" search.isrc
          ]
    case renderQueryParam qterms of
      Nothing -> pure V.empty
      Just q -> do
        let url = "/recording?query=" <> T.unpack q <> "&fmt=json"
        mbHttp @RecordingSearchResult url >>= \case
          Just r -> pure $ fromMaybe V.empty $ r.recordings
          Nothing -> do
            $(logError) "no matching recordings found"
            pure V.empty
  getArtist artistId = doCache (.getArtistStore) artistId do
    let url = "/artist/" <> T.unpack artistId.mbid <> "?inc=aliases&fmt=json"
    mbHttp url >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get artist " <> showt artistId.mbid
        pure Nothing
  getArtistReleaseGroups artistId = doCache (.getArtistReleaseGroupsStore) artistId do
    let params =
          [ ("artist", encodeUtf8 artistId.mbid),
            ("limit", "100"),
            ("inc", "artist-credits labels"),
            ("fmt", "json")
          ]
    let url = "/release-group/" <> renderSimpleQuery True params
    mbHttp (C8.unpack url) >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get release groups for artist " <> showt artistId.mbid
        pure V.empty
  getArtistReleases artistId = doCache (.getArtistReleasesStore) artistId do
    let params =
          [ ("artist", encodeUtf8 artistId.mbid),
            ("limit", "100"),
            ("inc", "artist-credits labels"),
            ("fmt", "json")
          ]
    let url = "/release/" <> renderSimpleQuery True params
    mbHttp (C8.unpack url) >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get releases for artist " <> showt artistId.mbid
        pure V.empty
  getArtistRecordings artistId = doCache (.getArtistRecordingsStore) artistId do
    let params =
          [ ("artist", encodeUtf8 artistId.mbid),
            ("limit", "100"),
            ("fmt", "json")
          ]
    let url = "/recording/" <> renderSimpleQuery True params
    mbHttp (C8.unpack url) >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get recordings for artist " <> showt artistId.mbid
        pure V.empty
  getRelease releaseId = doCache (.getReleaseStore) releaseId do
    let params =
          [ ("inc", "artist-credits labels"),
            ("fmt", "json")
          ]
    let url = "/release/" <> (encodeUtf8 releaseId.mbid) <> renderSimpleQuery True params
    mbHttp (C8.unpack url) >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get release " <> showt releaseId.mbid
        pure Nothing
  getReleaseGroup releaseGroupId = doCache (.getReleaseGroupStore) releaseGroupId do
    let params =
          [ ("inc", "artist-credits"),
            ("fmt", "json")
          ]
    let url = "/release-group/" <> (encodeUtf8 releaseGroupId.mbid) <> renderSimpleQuery True params
    mbHttp (C8.unpack url) >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get release group " <> showt releaseGroupId.mbid
        pure Nothing
  getRecording recordingId = doCache (.getRecordingStore) recordingId do
    let url = "/recording/" <> recordingId.mbid <> "?fmt=json"
    mbHttp (T.unpack url) >>= \case
      Just r -> pure r
      Nothing -> do
        $(logError) $ "failed to get recording " <> showt recordingId.mbid
        pure Nothing

data MusicBrainzConfig = MusicBrainzConfig
  { baseUrl :: String
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via FromGeneric MusicBrainzConfig

instance Default MusicBrainzConfig where
  def =
    MusicBrainzConfig
      { baseUrl = "https://musicbrainz.org/ws/2"
      }

instance FromJSON MusicBrainzConfig

instance ToJSON MusicBrainzConfig

musicbrainzConfigKey :: ConfigKey MusicBrainzConfig
musicbrainzConfigKey = ConfigKey "musicbrainz"

initMusicBrainzConfig :: (ConfigService m) => m ()
initMusicBrainzConfig = setConfig musicbrainzConfigKey def

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
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Artist Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Artist Id"
      }

originalArtistIdTag :: TagMapping
originalArtistIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ORIGINALARTISTID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ORIGINALARTISTID",
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Original Artist Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Original Artist Id"
      }

albumArtistIdTag :: TagMapping
albumArtistIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMARTISTID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMARTISTID",
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Album Artist Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Album Artist Id"
      }

albumIdTag :: TagMapping
albumIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Album Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Album Id"
      }

trackIdTag :: TagMapping
trackIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_RELEASETRACKID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_RELEASETRACKID",
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Release Track Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Release Track Id"
      }

recordingIdTag :: TagMapping
recordingIdTag =
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
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Release Track Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Release Track Id"
      }

releaseGroupIdTag :: TagMapping
releaseGroupIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_RELEASEGROUPID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_RELEASEGROUPID",
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Release Group Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Release Group Id"
      }

releaseIdTag :: TagMapping
releaseIdTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        vorbis = caseInsensitiveMapping "MUSICBRAINZ_ALBUMID",
        id3v2_3 = caseInsensitiveMapping "TXXX;MusicBrainz Album Id",
        id3v2_4 = caseInsensitiveMapping "TXXX;MusicBrainz Album Id"
      }
