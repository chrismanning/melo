{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Lookup.Covers (
  CoverService(..),
  CoverServiceIOT(..),
  Cover(..),
  CoverSource(..),
  SearchResult(..),
  ImageInfo(..),
  ImageFormat(..),
  runCoverServiceIO,
) where

import Conduit
import Control.Concurrent.STM (TVar, newTVar, readTVar, modifyTVar')
import Melo.Common.Exception
import Control.Lens hiding (from, lens, (<|))
import Control.Monad.Reader
import Data.Aeson as A
import Data.Aeson.Types
import Data.ByteString.Lazy as LBS
import Data.ByteString.Char8 as C
import Data.Conduit.ImageSize (sinkImageInfo)
import Data.Conduit.ImageSize qualified as ConduitImageSize
import Data.Foldable qualified as F
import Data.List.Extra (lower)
import Data.List.NonEmpty as NE
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Maybe as M
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Library.Release.Types
import Network.HTTP.Conduit as Http
import Network.HTTP.Types
import Streaming.Prelude qualified as S
import System.FilePath
import Text.Read hiding (lift)
import Witch

class Monad m => CoverService m where
  searchForCovers :: Release -> m [Cover]
  copyCoverToDir :: URI -> FilePath -> m ()

instance
  {-# OVERLAPPABLE #-}
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

data ImageFormat =
    GIF
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
      prependFailure "parsing Source failed, "
          (typeMismatch "String" invalid)

data SearchResult
  = Source
  | Done
  | Cover
      { smallCoverUrl :: Text,
        bigCoverUrl :: Text,
        source :: CoverSource
      }
  deriving (Generic, Show)

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

data CoverServiceData = CoverServiceData
  { httpManager :: Http.Manager,
    searchForCoversCache :: TVar (Map.HashMap Release [Cover]),
    copyCoverToDirCache :: TVar (Set.HashSet (String, FilePath))
  }

newtype CoverServiceIOT m a = CoverServiceIOT
  { runCoverServiceIOT :: ReaderT CoverServiceData m a
  }
  deriving newtype (
    Applicative,
    Functor,
    Monad,
    MonadBase b,
    MonadBaseControl b,
    MonadCatch,
    MonadConc,
    MonadIO,
    MonadMask,
    MonadReader CoverServiceData,
    MonadThrow,
    MonadTrans,
    MonadTransControl,
    PrimMonad
  )

instance
  ( MonadIO m,
    MonadCatch m,
    FileSystem m,
    Logging m
  ) =>
  CoverService (CoverServiceIOT m)
  where
  searchForCovers release = do
    cacheVar <- asks (.searchForCoversCache)
    cache <- liftIO $ atomically $ readTVar cacheVar
    case cache ^? at release . _Just of
      Just covers -> pure covers
      Nothing -> do
        manager <- asks (.httpManager)
        initialRequest <- parseRequest requestUri
        let !artist = fromMaybe "" $ release.artists ^? _head . #name
        let !requestObject = SearchRequest { artist, album = release.title, country = "gb", sources = (Bandcamp :| [Qobuz, Tidal]) }
        let !request = initialRequest { method = "POST", requestHeaders = [(hContentType, "application/json")], requestBody = RequestBodyLBS $ encode requestObject }
        $(logDebug) $ "Cover search request: " <> show request

        response <- liftIO $ httpLbs request manager
        results <- if responseStatus response == ok200 then let !bodies = LBS.split 0xA (responseBody response) in
              pure $ decode @SearchResult <$> bodies
            else do
              $(logError) $ "Response from cover search service: " <> show (responseStatus response)
              pure []
        covers <- S.each results
          & S.mapM \case
            Just Cover {..} -> do
              smallCover <- getImageInfo smallCoverUrl manager
              bigCover <- getImageInfo bigCoverUrl manager
              pure $ Just CoverInfo { smallCover, bigCover, source }
            _ -> pure Nothing
          & S.catMaybes
          & S.toList_
        liftIO $ atomically $ modifyTVar' cacheVar (at release ?~ covers)
        $(logInfo) $ ("Found " <> T.pack (show (F.length covers)) <> " covers for release " <> release.title)
        pure covers
    where
      getImageInfo url manager = liftIO $ runResourceT do
        request <- parseRequest $ T.unpack url
        $(logDebugIO) $ "getImageInfo request: " <> show request
        response <- http request manager
        $(logDebugIO) $ "getImageInfo response: " <> show (responseStatus response)
        !r <- runConduit $ responseBody response .| sinkImageInfo
        (size, format) <- r
        $(logDebugIO) $ "getImageInfo size: " <> show size
        let length = lookup hContentLength (responseHeaders response)
        $(logDebugIO) $ "getImageInfo length: " <> show length
        pure ImageInfo {
          url,
          format = from format,
          width = size.width,
          height = size.height,
          bytes = fromMaybe 0 (C.unpack <$> length >>= readMaybe)
        }
  copyCoverToDir src destDir = unlessM visited do
    $(logInfo) $ "copying cover from " <> show src <> " to " <> show destDir
    manager <- asks (.httpManager)
    handleErrors $ case uriScheme src of
      s | s == "http:" || s == "https:" -> do
        request <- parseRequest (show src)
        ext <- liftIO $ runResourceT do
          response <- http request manager
          r <- runConduit $ responseBody response .| sinkImageInfo
          getExt . snd <$> r
        let dest = destDir </> "cover" <> ext
        removePath dest >>= \case
          Left e -> $(logError) $ "failed to remove existing file " <> show dest <> ": " <> displayException e
          _ -> pure ()
        liftIO $ runResourceT do
          response <- http request manager
          runConduit $ responseBody response .| sinkFile (dest <> ".tmp")
        movePath (dest <> ".tmp") dest
        cacheVar <- asks (.copyCoverToDirCache)
        liftIO $ atomically $ modifyTVar' cacheVar (Set.insert (show src, destDir))
      "file:" -> do
        let srcPath = uriPath src
        whenM (doesFileExist srcPath) do
          r <- liftIO $ runConduitRes $ sourceFileBS srcPath .| sinkImageInfo
          ext <- getExt . snd <$> r
          let dest = destDir </> "cover" <> ext
          removePath dest >>= \case
            Left e -> $(logError) $ "failed to copy file " <> show srcPath <> " to " <> show dest <> ": " <> displayException e
            _ -> pure ()
          copyPath srcPath (dest <> ".tmp") >>= \case
            Left e -> $(logError) $ "failed to copy file " <> show srcPath <> " to " <> show dest <> ": " <> displayException e
            _ -> do
              movePath (dest <> ".tmp") dest
              cacheVar <- asks (.copyCoverToDirCache)
              liftIO $ atomically $ modifyTVar' cacheVar (Set.insert (show src, destDir))
              pure ()
      scheme -> do
        $(logWarn) $ "unsupported scheme " <> show scheme <> " in cover url"
        pure ()
      where
        handleErrors = handleAny \e -> do
          $(logError) $ "failed to copy cover from " <> show src <> " to " <> show destDir <> ": " <> displayException e
        getExt ConduitImageSize.GIF = ".gif"
        getExt ConduitImageSize.JPG = ".jpg"
        getExt ConduitImageSize.PNG = ".png"
        visited = do
          cacheVar <- asks (.copyCoverToDirCache)
          cache <- liftIO $ atomically $ readTVar cacheVar
          pure $ Set.member (show src, destDir) cache

runCoverServiceIO :: MonadIO m => Http.Manager -> CoverServiceIOT m a -> m a
runCoverServiceIO manager m = do
  (searchForCovers, copyCoverToDir) <- liftIO $ atomically ((,) <$> newTVar Map.empty <*> newTVar Set.empty)
  flip runReaderT (CoverServiceData manager searchForCovers copyCoverToDir) $ runCoverServiceIOT m
