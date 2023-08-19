{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Lookup.Covers
  ( CoverService (..),
    Cover (..),
    CoverSource (..),
    SearchResult (..),
    ImageInfo (..),
    ImageFormat (..),
  )
where

import Conduit
import Control.Concurrent.Classy.STM
import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.Char8 as C
import Data.ByteString.Streaming.Aeson qualified as SA
import Data.ByteString.Streaming.HTTP qualified as SH
import Data.Conduit.ImageSize (sinkImageInfo)
import Data.Conduit.ImageSize qualified as ConduitImageSize
import Data.Foldable qualified as F
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.List.Extra (lower)
import Data.String (IsString)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.Http as Http
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Common.Uri
import Melo.Library.Release.Types
import Network.HTTP.Conduit as Http
import Network.HTTP.Types
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Instrumentation.HttpClient.Raw qualified as OtelHttp
import Streaming.Prelude qualified as S
import System.FilePath
import Text.EditDistance
import Text.Read hiding (lift)

class Monad m => CoverService m where
  searchForCovers :: Release -> m [Cover]
  copyCoverToDir :: URI -> FilePath -> m ()

instance
  {-# OVERLAPS #-}
  ( Monad (t m),
    MonadTrans t,
    CoverService m
  ) =>
  CoverService (t m)
  where
  searchForCovers = lift . searchForCovers
  copyCoverToDir src dest = lift (copyCoverToDir src dest)

data Cover = CoverInfo
  { smallCover :: ImageInfo,
    bigCover :: ImageInfo,
    source :: CoverSource
  }
  deriving (Generic)

instance ToJSON Cover

data ImageInfo = ImageInfo
  { width :: Int,
    height :: Int,
    format :: ImageFormat,
    bytes :: Int,
    url :: Text
  }
  deriving (Generic)

instance ToJSON ImageInfo

data ImageFormat
  = GIF
  | JPG
  | PNG
  deriving (Generic)

instance ToJSON ImageFormat

instance From ConduitImageSize.FileFormat ImageFormat where
  from ConduitImageSize.GIF = GIF
  from ConduitImageSize.JPG = JPG
  from ConduitImageSize.PNG = PNG

requestUri :: IsString s => s
requestUri = "https://covers.musichoarders.xyz/api/search"

data SearchRequest = SearchRequest
  { artist :: Text,
    album :: Text,
    country :: Text,
    sources :: NonEmpty CoverSource
  }
  deriving (Generic)

instance ToJSON SearchRequest where
  toEncoding = genericToEncoding defaultOptions

data CoverSource
  = Bandcamp
  | Qobuz
  | Tidal
  deriving (Show)
  deriving (TextShow) via FromStringShow CoverSource

instance ToJSON CoverSource where
  toJSON Bandcamp = toJSON @String "bandcamp"
  toJSON Qobuz = toJSON @String "qobuz"
  toJSON Tidal = toJSON @String "tidal"
  toEncoding Bandcamp = toEncoding @String "bandcamp"
  toEncoding Qobuz = toEncoding @String "qobuz"
  toEncoding Tidal = toEncoding @String "tidal"

instance FromJSON CoverSource where
  parseJSON (A.String "bandcamp") = pure Bandcamp
  parseJSON (A.String "qobuz") = pure Qobuz
  parseJSON (A.String "tidal") = pure Tidal
  parseJSON invalid =
    prependFailure
      "parsing Source failed, "
      (typeMismatch "String" invalid)

data SearchResult
  = Source
  | Done
  | Cover
      { smallCoverUrl :: Text,
        bigCoverUrl :: Text,
        releaseInfo :: Maybe ReleaseInfo,
        source :: CoverSource
      }
  deriving (Generic, Show)
  deriving (TextShow) via FromGeneric SearchResult

instance FromJSON SearchResult where
  parseJSON =
    genericParseJSON
      defaultOptions
        { sumEncoding =
            TaggedObject
              { tagFieldName = "type",
                contentsFieldName = ""
              },
          constructorTagModifier = lower
        }

data ReleaseInfo = ReleaseInfo
  { title :: Maybe String,
    artist :: Maybe String
  }
  deriving (Generic, Show)
  deriving (TextShow) via FromGeneric ReleaseInfo

instance FromJSON ReleaseInfo

data CoverServiceData = CoverServiceData
  { searchForCoversCache :: TVar (STM IO) (Map.HashMap Release [Cover]),
    copyCoverToDirCache :: TVar (STM IO) (Set.HashSet (String, FilePath))
  }
  deriving (Typeable)

getCacheData :: (MonadIO m, AppDataReader m) => m CoverServiceData
getCacheData =
  getAppData >>= \case
    Just cacheData -> pure cacheData
    Nothing -> do
      cacheData <- liftIO $ atomically $ CoverServiceData <$> newTVar Map.empty <*> newTVar Set.empty
      putAppData cacheData
      pure cacheData

instance CoverService (AppM IO IO) where
  searchForCovers release = handleErrors $ withSpan "searchForCovers" defaultSpanArguments do
    cacheVar <- getCacheData <&> (.searchForCoversCache)
    cache <- liftIO $ atomically $ readTVar cacheVar
    case cache ^? at release . _Just of
      Just covers -> pure covers
      Nothing -> runResourceT do
        manager <- Http.getManager
        initialRequest <- parseRequest requestUri
        let !artist = fromMaybe "" $ release.artists ^? _head . #name
        let !requestObject = SearchRequest {artist, album = release.title, country = "gb", sources = (Bandcamp :| [Qobuz, Tidal])}
        let !request = initialRequest {method = "POST", requestHeaders = [(hContentType, "application/json")], requestBody = RequestBodyLBS $ encode requestObject}
        let clientRequestUrl = T.pack requestUri
        $(logInfoV ['clientRequestUrl]) "Searching for images"

        -- TODO otel http while streaming
        response <- SH.http request manager

        if not (statusIsSuccessful response.responseStatus)
          then do
            let clientResponseStatus = show response.responseStatus
            $(logErrorV ['clientRequestUrl, 'clientResponseStatus]) "Response received from cover search"
            pure []
          else do
            covers <- response.responseBody
              & SA.decoded
              & S.mapM \case
                Just Cover {..} | matches releaseInfo artist -> do
                  smallCover <- getImageInfo smallCoverUrl manager
                  bigCover <- getImageInfo bigCoverUrl manager
                  pure $ Just CoverInfo {smallCover, bigCover, source}
                _ -> pure Nothing
              & S.catMaybes
              & S.toList_
            liftIO $ atomically $ modifyTVar' cacheVar (at release ?~ covers)
            $(logInfo) $ "Found " <> showt (F.length covers) <> " covers for release " <> release.title
            pure covers
    where
      getImageInfo url manager = do
        request <- parseRequest $ T.unpack url
        let clientRequestUrl = url
        $(logInfoVIO ['clientRequestUrl]) "Getting image to determine size"
        response <- http request manager
        let clientResponseStatus = show response.responseStatus
        $(logDebugVIO ['clientResponseStatus]) "HTTP client received response"
        !r <- runConduit $ responseBody response .| sinkImageInfo
        (size, format) <- r
        $(logInfoIO) $ "Image size: " <> T.pack (show size)
        let length = lookup hContentLength (responseHeaders response)
        $(logDebugIO) $ "getImageInfo length: " <> showt length
        pure
          ImageInfo
            { url,
              format = from format,
              width = size.width,
              height = size.height,
              bytes = fromMaybe 0 (C.unpack <$> length >>= readMaybe)
            }
      matches :: Maybe ReleaseInfo -> Text -> Bool
      matches (Just ReleaseInfo {artist=Just artist, title=Just title}) artistName =
        levenshteinDistance defaultEditCosts (T.unpack artistName) artist < 4
        && levenshteinDistance defaultEditCosts (T.unpack release.title) title < 4
      matches _ _ = False
      handleErrors = handleHttp \e -> do
        let cause = show e
        $(logErrorV ['cause]) "Failed to search for covers"
        pure []
  copyCoverToDir src destDir = unlessM visited do
    $(logInfo) $ "copying cover from " <> showt src <> " to " <> showt destDir
    manager <- Http.getManager
    handleErrors $ case uriScheme src of
      s | s == "http:" || s == "https:" -> do
        request <- parseRequest (show src)
        ext <- withSpan "http" Http.spanArgs $ liftIO $ runResourceT do
          context <- Context.getContext
          request' <- OtelHttp.instrumentRequest Http.otelConf context request
          response <- http request' manager
          OtelHttp.instrumentResponse Http.otelConf context response
          r <- runConduit $ responseBody response .| sinkImageInfo
          getExt . snd <$> r
        let dest = destDir </> "cover" <> ext
        removePath dest >>= \case
          Left e ->
            let cause = displayException e
             in $(logErrorV ['cause]) $ "failed to remove existing file " <> showt dest
          _ -> pure ()
        withSpan "http" Http.spanArgs $ liftIO $ runResourceT do
          context <- Context.getContext
          request' <- OtelHttp.instrumentRequest Http.otelConf context request
          response <- http request' manager
          OtelHttp.instrumentResponse Http.otelConf context response
          runConduit $ responseBody response .| sinkFile (dest <> ".tmp")
        movePath (dest <> ".tmp") dest
        cacheVar <- getCacheData <&> (.copyCoverToDirCache)
        liftIO $ atomically $ modifyTVar' cacheVar (Set.insert (show src, destDir))
      "file:" -> do
        let srcPath = uriPath src
        whenM (doesFileExist srcPath) do
          r <- liftIO $ runConduitRes $ sourceFileBS srcPath .| sinkImageInfo
          ext <- getExt . snd <$> r
          let dest = destDir </> "cover" <> ext
          removePath dest >>= \case
            Left e ->
              let cause = displayException e
               in $(logErrorV ['cause]) $ "failed to copy file " <> showt srcPath <> " to " <> showt dest
            _ -> pure ()
          copyPath srcPath (dest <> ".tmp") >>= \case
            Left e ->
              let cause = displayException e
               in $(logErrorV ['cause]) $ "failed to copy file " <> showt srcPath <> " to " <> showt dest
            _ -> do
              movePath (dest <> ".tmp") dest
              cacheVar <- getCacheData <&> (.copyCoverToDirCache)
              liftIO $ atomically $ modifyTVar' cacheVar (Set.insert (show src, destDir))
              pure ()
      scheme -> do
        $(logWarn) $ "unsupported scheme " <> showt scheme <> " in cover url"
        pure ()
    where
      handleErrors = handleAny \e -> do
        let cause = displayException e
        $(logErrorV ['cause]) $ "failed to copy cover from " <> showt src <> " to " <> showt destDir
      getExt ConduitImageSize.GIF = ".gif"
      getExt ConduitImageSize.JPG = ".jpg"
      getExt ConduitImageSize.PNG = ".png"
      visited = do
        cacheVar <- getCacheData <&> (.copyCoverToDirCache)
        cache <- liftIO $ atomically $ readTVar cacheVar
        pure $ Set.member (show src, destDir) cache
