{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Lookup.MusicBrainz
  ( MusicBrainzId (..),
    runMusicBrainzServiceIO,
    runMusicBrainzServiceUnlimitedIO,
    MusicBrainzService (..),
    MusicBrainzServiceIOT (..),
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
    CachingMusicBrainzServiceT (..),
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
    runCachingMusicBrainzService,
  )
where

import Control.Applicative as A
import Control.Concurrent.Classy
import Control.Concurrent.TokenLimiter
import Control.Exception.Safe
import Control.Foldl qualified as F
import Control.Lens hiding (from, lens)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.Aeson.Types
import Data.Char
import Data.Default
import Data.Generics.Labels ()
import Data.Map.Strict (Map)
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector ())
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Records
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.RateLimit
import Melo.Format.Mapping
  ( FieldMappings (..),
    TagMapping (..),
    caseInsensitiveMapping,
    singletonTagMapping,
  )
import Melo.Format.Mapping qualified as M
import Melo.Format.Metadata qualified as F
import Network.Wreq qualified as Wr
import Network.Wreq.Session qualified as WrS
import Streaming.Prelude qualified as S
import Prelude as P

newtype MusicBrainzId = MusicBrainzId
  { mbid :: Text
  }
  deriving (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON)

newtype ArtistSearch = ArtistSearch
  { artist :: Text
  }
  deriving stock (Show, Generic, Eq, Ord)

newtype ArtistSearchResult = ArtistSearchResult
  { artists :: Maybe (Vector Artist)
  }
  deriving (Show, Generic, Eq)

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

instance Ord Artist where
  a <= b = a.id <= b.id && a.name <= b.name

instance FromJSON Artist where
  parseJSON = genericParseJSON mbAesonOptions

data ArtistAlias = ArtistAlias
  { name :: Text,
    sortName :: Maybe Text,
    type' :: Maybe Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON ArtistAlias where
  parseJSON = withObject "ArtistAlias" $ \v ->
    ArtistAlias
      <$> v .: "name"
      <*> v .:? "sort-name"
      <*> v .:? "type"

data Area = Area
  { iso3166_2codes :: Maybe (Vector CountrySubdivision),
    iso3166_1codes :: Maybe (Vector Text)
  }
  deriving (Show, Generic, Eq)

instance FromJSON Area where
  parseJSON = withObject "Area" $ \v ->
    Area
      <$> v .:? "iso-3166-2-codes"
      <*> v .:? "iso-3166-1-codes"

data CountrySubdivision = CountrySubdivision
  { country :: Text,
    subdivision :: Text
  }
  deriving (Show, Eq)

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

data ReleaseSearch = ReleaseSearch
  { albumArtist :: Maybe Text,
    albumTitle :: Maybe Text,
    date :: Maybe Text,
    catNum :: Maybe Text
  }
  deriving (Show, Generic, Eq, Ord)

instance Default ReleaseSearch

newtype ReleaseSearchResult = ReleaseSearchResult
  { releases :: Maybe (Vector Release)
  }
  deriving (Show, Generic, Eq)

instance FromJSON ReleaseSearchResult

data Release = Release
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe (Vector ArtistCredit),
    date :: Maybe Text,
    score :: Maybe Int,
    labelInfo :: Maybe (Vector LabelInfo)
  }
  deriving (Show, Generic, Eq)

instance FromJSON Release where
  parseJSON = genericParseJSON mbAesonOptions

data ArtistCredit = ArtistCredit
  { name :: Maybe Text,
    artist :: Artist
  }
  deriving (Show, Generic, Eq, Ord)

instance FromJSON ArtistCredit where
  parseJSON = genericParseJSON mbAesonOptions

data LabelInfo = LabelInfo
  { catalogNumber :: Maybe Text,
    label :: Label
  }
  deriving (Show, Generic, Eq)

instance FromJSON LabelInfo where
  parseJSON = genericParseJSON mbAesonOptions

data Label = Label
  { id :: MusicBrainzId,
    name :: Text
  }
  deriving (Show, Generic, Eq)

instance FromJSON Label where
  parseJSON = genericParseJSON mbAesonOptions

newtype ReleaseGroupSearchResult = ReleaseGroupSearchResult
  { releaseGroups :: Maybe (Vector ReleaseGroup)
  }
  deriving (Show, Generic, Eq)

instance FromJSON ReleaseGroupSearchResult where
  parseJSON = genericParseJSON mbAesonOptions

data ReleaseGroup = ReleaseGroup
  { id :: MusicBrainzId,
    title :: Text,
    artistCredit :: Maybe (Vector ArtistCredit),
    firstReleaseDate :: Maybe Text,
    score :: Maybe Int
  }
  deriving (Show, Generic, Eq)

instance FromJSON ReleaseGroup where
  parseJSON = genericParseJSON mbAesonOptions

data RecordingSearch = RecordingSearch
  { title :: Maybe Text,
    trackNumber :: Maybe Text,
    artist :: Maybe Text,
    album :: Maybe Text,
    releaseId :: Maybe Text,
    releaseGroupId :: Maybe Text,
    isrc :: Maybe Text
  }
  deriving (Show, Generic, Eq, Ord)

instance Default RecordingSearch

newtype RecordingSearchResult = RecordingSearchResult
  { recordings :: Maybe (Vector Recording)
  }
  deriving (Show, Generic, Eq)

instance FromJSON RecordingSearchResult

data Recording = Recording
  { id :: MusicBrainzId,
    score :: Maybe Int,
    title :: Text,
    artistCredit :: Maybe (Vector ArtistCredit),
    releases :: Vector Release
  }
  deriving (Show, Generic, Eq)

instance FromJSON Recording where
  parseJSON = genericParseJSON mbAesonOptions

class Monad m => MusicBrainzService m where
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
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe (Either Release ReleaseGroup))
getReleaseOrGroup m =
  getReleaseFromMetadata m >>= \case
    Just release -> pure $ Just $ Left release
    Nothing -> fmap Right <$> getReleaseGroupFromMetadata m

getReleaseAndGroup ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe ReleaseGroup, Maybe Release)
getReleaseAndGroup m = do
  releaseGroup <- getReleaseGroupFromMetadata m
  release <- getReleaseFromMetadata m
  pure (releaseGroup, release)

getReleaseFromMetadata ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Release)
getReleaseFromMetadata m =
  getReleaseByMusicBrainzId m <<|>> getReleaseByAlbum m

getReleaseByMusicBrainzId ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Release)
getReleaseByMusicBrainzId m = do
  let releaseIds = MusicBrainzId <$> m.tag releaseIdTag
  S.each releaseIds
    & S.mapMaybeM getRelease
    & F.impurely S.foldM_ (F.generalize $ F.find perfectScore)

getReleaseByAlbum ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Release)
getReleaseByAlbum m = do
  let albumTitle = m.tagHead M.album
  let albumArtists = m.tag M.albumArtist
  let date = m.tagHead M.originalReleaseYear <&> truncateDate
  let catNum = m.tagHead M.catalogNumber
  if isJust albumTitle && not (null albumArtists) && isJust catNum
    then
      find perfectScore
        <$> searchReleases def {albumArtist = Just $ T.intercalate " & " (toList albumArtists), albumTitle, date, catNum}
    else pure Nothing

getReleaseGroupFromMetadata ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe ReleaseGroup)
getReleaseGroupFromMetadata m =
  getReleaseGroupByMusicBrainzId m <<|>> getReleaseGroupByAlbum m

getReleaseGroupByMusicBrainzId ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe ReleaseGroup)
getReleaseGroupByMusicBrainzId m = do
  let releaseGroupIds = MusicBrainzId <$> m.tag releaseGroupIdTag
  S.each releaseGroupIds
    & S.mapMaybeM getReleaseGroup
    & F.impurely S.foldM_ (F.generalize $ F.find perfectScore)

getReleaseGroupByAlbum ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe ReleaseGroup)
getReleaseGroupByAlbum m = do
  let albumTitle = m.tagHead M.album
  let albumArtists = m.tag M.albumArtist
  let date = m.tagHead M.originalReleaseYear <&> truncateDate
  if isJust albumTitle && not (null albumArtists)
    then
      find perfectScore
        <$> searchReleaseGroups def {albumArtist = Just $ T.intercalate " & " (toList albumArtists), albumTitle, date}
    else pure Nothing

getRecordingFromMetadata ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingFromMetadata m =
  getRecordingByMusicBrainzId m
    <<|>> getRecordingByReleaseIdTags m
    <<|>> getRecordingByTrack m

getRecordingByMusicBrainzId ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingByMusicBrainzId m = do
  let recordingIds = m.tag recordingIdTag
  V.find perfectScore . V.catMaybes
    <$> V.forM recordingIds (getRecording . MusicBrainzId)

getRecordingByReleaseIdTags ::
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingByReleaseIdTags m = do
  let title = m.tagHead M.trackTitle
  let releaseId = m.tagHead releaseIdTag
  let releaseGroupId = m.tagHead releaseGroupIdTag
  if isJust releaseId || isJust releaseGroupId
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
  MusicBrainzService m =>
  F.Metadata ->
  m (Maybe Recording)
getRecordingByTrack m = do
  let title = m.tagHead M.trackTitle
  let album = m.tagHead M.album
  let trackNumber = m.tagHead M.trackNumber
  let artist = m.tagHead M.artist
  let isrc = m.tagHead M.isrc
  if isJust title && (isJust album || isJust artist)
    then find perfectScore <$> searchRecordings def {title, album, trackNumber, artist, isrc}
    else pure Nothing

perfectScore :: (Integral i, HasField "score" r (Maybe i)) => r -> Bool
perfectScore r = r.score == Just 100

truncateDate :: Text -> Text
truncateDate = T.takeWhile isDigit

newtype MusicBrainzServiceIOT m a = MusicBrainzServiceIOT
  { runMusicBrainzServiceIOT :: HttpSessionIOT m a
  }
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadBase b,
      MonadBaseControl b,
      MonadCatch,
      MonadConc,
      MonadIO,
      MonadMask,
      MonadThrow,
      PrimMonad
    )

instance MonadTrans MusicBrainzServiceIOT where
  lift m = MusicBrainzServiceIOT $ HttpSessionIOT $ ReaderT $ const m

instance
  ( MonadIO m,
    RateLimit m,
    Logging m
  ) =>
  MusicBrainzService (MusicBrainzServiceIOT m)
  where
  searchReleases search = MusicBrainzServiceIOT $ do
    waitReady
    let qterms =
          catMaybes
            [ fmap (\t -> "releaseaccent:\"" <> t <> "\"") search.albumTitle,
              fmap (\a -> "artist:\"" <> a <> "\"") search.albumArtist,
              fmap (\c -> "catno:\"" <> c <> "\"") search.catNum
            ]
    case qterms of
      [] -> pure V.empty
      ts -> do
        let q = T.intercalate " AND " ts
        let opts = mbWreqDefaults
        let url = baseUrl <> "/release?query=" <> q
        getWithJson @_ @ReleaseSearchResult opts url >>= \case
          Left e -> do
            $(logError) $ "error searching for matching releases: " <> show e
            pure V.empty
          Right r -> pure $ fromMaybe V.empty $ r ^. Wr.responseBody . #releases
  searchReleaseGroups search = MusicBrainzServiceIOT $ do
    waitReady
    let qterms =
          catMaybes
            [ fmap (\t -> "(releasegroupaccent:\"" <> t <> "\" OR title:\"" <> t <> "\")") search.albumTitle,
              fmap (\a -> "artist:\"" <> a <> "\"") search.albumArtist,
              fmap (\a -> "firstreleasedate:\"" <> a <> "\"") search.date
            ]
    case qterms of
      [] -> pure V.empty
      ts -> do
        let q = T.intercalate " AND " ts
        let opts = mbWreqDefaults
        let url = baseUrl <> "/release-group?query=" <> q
        getWithJson @_ @ReleaseGroupSearchResult opts url >>= \case
          Left e -> do
            $(logError) $ "error searching for matching release groups: " <> show e
            pure V.empty
          Right r -> pure $ fromMaybe V.empty $ r ^. Wr.responseBody . #releaseGroups
  searchArtists search = MusicBrainzServiceIOT $ do
    waitReady
    let opts = mbWreqDefaults
    let url = baseUrl <> "/artist?query=" <> "artist:\"" <> search.artist <> "\""
    getWithJson @_ @ArtistSearchResult opts url >>= \case
      Left e -> do
        $(logError) $ "error searching for matching artists: " <> show e
        pure V.empty
      Right r -> pure $ fromMaybe V.empty $ r ^. Wr.responseBody . #artists
  searchRecordings search = MusicBrainzServiceIOT $ do
    waitReady
    let qterms =
          catMaybes
            [ fmap (\a -> "recording:\"" <> a <> "\"") search.title,
              fmap (\a -> "tnum:\"" <> a <> "\"") search.trackNumber,
              fmap (\a -> "artist:\"" <> a <> "\"") search.artist,
              fmap (\a -> "release:\"" <> a <> "\"") search.album,
              fmap (\a -> "reid:\"" <> a <> "\"") search.releaseId,
              fmap (\a -> "rgid:\"" <> a <> "\"") search.releaseGroupId,
              fmap (\a -> "isrc:\"" <> a <> "\"") search.isrc
            ]
    case qterms of
      [] -> pure V.empty
      ts -> do
        let q = T.intercalate " AND " ts
        let opts = mbWreqDefaults
        let url = baseUrl <> "/recording?query=" <> q
        getWithJson @_ @RecordingSearchResult opts url >>= \case
          Left e -> do
            $(logError) $ "error searching for matching recordings: " <> show e
            pure V.empty
          Right r -> pure $ fromMaybe V.empty $ r ^. Wr.responseBody . #recordings
  getArtist artistId = MusicBrainzServiceIOT $ do
    waitReady
    let opts = mbWreqDefaults
    let url = baseUrl <> "/artist/" <> artistId.mbid <> "?inc=aliases"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting artist " <> show artistId.mbid <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody
  getArtistReleaseGroups artistId = MusicBrainzServiceIOT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "artist" .~ [artistId.mbid]
            & Wr.param "limit" .~ ["100"]
            & Wr.param "inc" .~ ["artist-credits labels"]
    let url = baseUrl <> "/release-group/"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting release groups for artist " <> show artistId.mbid <> ": " <> show e
        pure V.empty
      Right r -> pure $ r ^. Wr.responseBody
  getArtistReleases artistId = MusicBrainzServiceIOT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "artist" .~ [artistId.mbid]
            & Wr.param "limit" .~ ["100"]
            & Wr.param "inc" .~ ["artist-credits labels"]
    let url = baseUrl <> "/release/"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting releases for artist " <> show artistId.mbid <> ": " <> show e
        pure V.empty
      Right r -> pure $ r ^. Wr.responseBody
  getArtistRecordings artistId = MusicBrainzServiceIOT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "artist" .~ [artistId.mbid]
            & Wr.param "limit" .~ ["100"]
    let url = baseUrl <> "/recording/"
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting recordings for artist " <> show artistId.mbid <> ": " <> show e
        pure V.empty
      Right r -> pure $ r ^. Wr.responseBody
  getRelease releaseId = MusicBrainzServiceIOT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "inc" .~ ["artist-credits labels"]
    let url = baseUrl <> "/release/" <> releaseId.mbid
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting release " <> show releaseId.mbid <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody
  getReleaseGroup releaseGroupId = MusicBrainzServiceIOT $ do
    waitReady
    let opts =
          mbWreqDefaults
            & Wr.param "inc" .~ ["artist-credits"]
    let url = baseUrl <> "/release-group/" <> releaseGroupId.mbid
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting release group " <> show releaseGroupId.mbid <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody
  getRecording recordingId = MusicBrainzServiceIOT $ do
    waitReady
    let opts = mbWreqDefaults
    let url = baseUrl <> "/recording/" <> recordingId.mbid
    getWithJson opts url >>= \case
      Left e -> do
        $(logError) $ "error getting recording " <> show recordingId.mbid <> ": " <> show e
        pure Nothing
      Right r -> pure $ r ^. Wr.responseBody

mbWreqDefaults :: Wr.Options
mbWreqDefaults =
  Wr.defaults
    & Wr.header "User-Agent" .~ [meloUserAgent]
    & Wr.header "Accept" .~ ["application/json"]
    & Wr.param "fmt" .~ ["json"]

baseUrl :: IsString s => s
-- TODO configurable musicbrainz url
-- baseUrl = "https://musicbrainz.org/ws/2"
baseUrl = "http://192.168.1.170:5000/ws/2"

runMusicBrainzServiceIO ::
  ( MonadIO m
  ) =>
  WrS.Session ->
  MusicBrainzServiceIOT (RateLimitIOT m) a ->
  m a
runMusicBrainzServiceIO sess =
  runRateLimitIO mbRateLimitConfig
    . runHttpSession sess
    . runMusicBrainzServiceIOT

mbRateLimitConfig :: LimitConfig
mbRateLimitConfig =
  defaultLimitConfig
    { maxBucketTokens = 1,
      initialBucketTokens = 1,
      bucketRefillTokensPerSecond = 1
    }

runMusicBrainzServiceUnlimitedIO ::
  WrS.Session ->
  MusicBrainzServiceIOT (UnlimitedT m) a ->
  m a
runMusicBrainzServiceUnlimitedIO sess =
  runUnlimitedT
    . runHttpSession sess
    . runMusicBrainzServiceIOT

newtype UnlimitedT m a = UnlimitedT {runUnlimitedT :: m a}
  deriving (MonadTrans, MonadTransControl) via IdentityT
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      PrimMonad
    )

instance Monad m => RateLimit (UnlimitedT m) where
  waitReady = pure ()

data CacheAggregate = CacheAggregate
  { searchReleasesStore :: Map ReleaseSearch (Vector Release),
    searchReleaseGroupsStore :: Map ReleaseSearch (Vector ReleaseGroup),
    searchArtistsStore :: Map ArtistSearch (Vector Artist),
    searchRecordingsStore :: Map RecordingSearch (Vector Recording),
    getArtistStore :: Map MusicBrainzId (Maybe Artist),
    getArtistReleaseGroupsStore :: Map MusicBrainzId (Vector ReleaseGroup),
    getArtistReleasesStore :: Map MusicBrainzId (Vector Release),
    getArtistRecordingsStore :: Map MusicBrainzId (Vector Recording),
    getReleaseStore :: Map MusicBrainzId (Maybe Release),
    getReleaseGroupStore :: Map MusicBrainzId (Maybe ReleaseGroup),
    getRecordingStore :: Map MusicBrainzId (Maybe Recording)
  }
  deriving (Generic)

instance Default CacheAggregate where

newtype CachingMusicBrainzServiceT m a = CachingMusicBrainzServiceT
  { runCachingMusicBrainzServiceT :: StateT CacheAggregate m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

doCache :: MusicBrainzService m => Lens' CacheAggregate (Maybe a) -> m a -> StateT CacheAggregate m a
doCache l m = doImpl $ lift $ m
  where
    doImpl mb = use l >>= \case
      Just x -> pure x
      Nothing -> mb >>= (l <?=)

instance MusicBrainzService m => MusicBrainzService (CachingMusicBrainzServiceT m) where
  searchReleases search = CachingMusicBrainzServiceT $ doCache ( #searchReleasesStore . at search ) (searchReleases search)
  searchReleaseGroups search = CachingMusicBrainzServiceT $ doCache ( #searchReleaseGroupsStore . at search ) (searchReleaseGroups search)
  searchArtists search = CachingMusicBrainzServiceT $ doCache ( #searchArtistsStore . at search ) (searchArtists search)
  searchRecordings search = CachingMusicBrainzServiceT $ doCache ( #searchRecordingsStore . at search ) (searchRecordings search)
  getArtist mbid = CachingMusicBrainzServiceT $ doCache ( #getArtistStore . at mbid ) (getArtist mbid)
  getArtistReleaseGroups mbid = CachingMusicBrainzServiceT $ doCache ( #getArtistReleaseGroupsStore . at mbid ) (getArtistReleaseGroups mbid)
  getArtistReleases mbid = CachingMusicBrainzServiceT $ doCache ( #getArtistReleasesStore . at mbid ) (getArtistReleases mbid)
  getArtistRecordings mbid = CachingMusicBrainzServiceT $ doCache ( #getArtistRecordingsStore . at mbid ) (getArtistRecordings mbid)
  getRelease mbid = CachingMusicBrainzServiceT $ doCache ( #getReleaseStore . at mbid ) (getRelease mbid)
  getReleaseGroup mbid = CachingMusicBrainzServiceT $ doCache ( #getReleaseGroupStore . at mbid ) (getReleaseGroup mbid)
  getRecording mbid = CachingMusicBrainzServiceT $ doCache ( #getRecordingStore . at mbid ) (getRecording mbid)

runCachingMusicBrainzService :: Monad m => CachingMusicBrainzServiceT m a -> m a
runCachingMusicBrainzService (CachingMusicBrainzServiceT m) = evalStateT m def

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
