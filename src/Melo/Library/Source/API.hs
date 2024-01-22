{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Control.Concurrent.Classy hiding (catch, newChan, readChan, writeChan)
import Control.DeepSeq
import Control.Exception.Safe hiding (finally, throw)
import Control.Foldl qualified as Fold
import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as L
import Data.Coerce
import Data.Either.Combinators
import Data.Generics.Labels ()
import Data.Kind
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock
import Data.UUID (fromText, toText)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Melo.Common.API
import Melo.Common.Config
import Melo.Common.Exception qualified as E
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Common.Tracing
import Melo.Common.Uri
import Melo.Common.Uuid
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO (selectStream)
import Melo.Format qualified as F
import Melo.Library.Artist.Name.Repo
import Melo.Library.Collection.Repo (CollectionRepository)
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Release.Aggregate as Release
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo
import Melo.Library.Release.Types
import Melo.Library.Source.Aggregate
import Melo.Library.Source.Repo
import Melo.Library.Source.Transform qualified as Tr
import Melo.Library.Source.Types qualified as Ty
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo as Track
import Melo.Library.Track.Types
import Melo.Lookup.Covers (Cover (..), CoverService (..))
import Melo.Lookup.Covers qualified as Covers
import Melo.Metadata.Aggregate
import Melo.Metadata.Mapping.Aggregate
import Network.RSocket qualified as RSocket
import Rel8 ((&&.), (==.))
import Rel8 qualified
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath as P

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "downloadCover") (jsonRqRawRsRoute getCoverImage)
  registerRoute (RouteKey "getSources") (jsonRqJsonStreamRoute streamSourceGroups)
  registerRoute (RouteKey "previewTransformSources") (jsonRqJsonStreamRoute previewTransformSources)
  registerRoute (RouteKey "transformSources") (jsonRqJsonStreamRoute transformSources)
  pure ()

-- TODO API getting specific (mapped) tags for source(s)
resolveMappedTags :: (TagMappingAggregate m, Tracing m) => Ty.Source -> Vector Text -> m Ty.MappedTags
resolveMappedTags src mappingNames = withSpan "resolveMappedTags" defaultSpanArguments do
  forM mappingNames $ \mappingName -> do
    values <- resolveMappingNamed mappingName src
    pure
      Ty.MappedTag
        { mappingName,
          values
        }

-- TODO cover image search API
data ImageSearchResult = ImageSearchResult
  { smallCover :: ImageInfo,
    bigCover :: ImageInfo,
    source :: CoverSource
  }
  deriving (Generic)
  deriving (ToJSON) via CustomJSON JSONOptions ImageSearchResult

instance From Cover ImageSearchResult where
  from CoverInfo {..} =
    ImageSearchResult
      { bigCover = from bigCover,
        smallCover = from smallCover,
        source = from source
      }

data CoverSource
  = FileSystem
  | Bandcamp
  | Qobuz
  | Tidal
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake, ToLower]] CoverSource

instance From Covers.CoverSource CoverSource where
  from Covers.Bandcamp = Bandcamp
  from Covers.Qobuz = Qobuz
  from Covers.Tidal = Tidal

data ImageInfo = ImageInfo
  { width :: Int,
    height :: Int,
    url :: Text,
    bytes :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via FromGeneric ImageInfo
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions ImageInfo

instance From Covers.ImageInfo ImageInfo where
  from s =
    ImageInfo
      { width = s.width,
        height = s.height,
        url = s.url,
        bytes = s.bytes
      }

data TransformSources = TransformSources
  { transformations :: Vector Tr.TransformAction,
    sources :: Vector Ty.SourceRef
  }
  deriving (Generic)
  deriving (FromJSON) via CustomJSON JSONOptions TransformSources

data UpdatedSourceResult
  = UpdatedSource Ty.Source
  | FailedSourceUpdate {id :: Ty.SourceRef, msg :: Text}
  deriving (Generic)
  deriving (ToJSON) via CustomJSON JSONOptions UpdatedSourceResult

previewTransformSources :: TransformSources -> ContT () (AppM IO IO) (S.Stream (S.Of UpdatedSourceResult) (AppM IO IO) ())
previewTransformSources = transformSourcesImpl (Tr.previewTransformation . Tr.evalTransformActions) True

transformSources :: TransformSources -> ContT () (AppM IO IO) (S.Stream (S.Of UpdatedSourceResult) (AppM IO IO) ())
transformSources = transformSourcesImpl Tr.evalTransformActions False

transformSourcesImpl :: (Vector Tr.TransformAction -> Tr.Transform (AppM IO IO)) -> Bool -> TransformSources -> ContT () (AppM IO IO) (S.Stream (S.Of UpdatedSourceResult) (AppM IO IO) ())
transformSourcesImpl transformer preview req = selectStream (sourcesByKeys req.sources) <&> S.mapM transformSourceImpl
  where
    spanTag = if preview then "previewTransformSource" else "transformSource"
    transformSourceImpl :: Ty.SourceEntity -> AppM IO IO UpdatedSourceResult
    transformSourceImpl s = withSpan spanTag defaultSpanArguments do
      if preview
        then $(logDebug) $ "Previewing source transformation of " <> showt s.id <> " with " <> showt req.transformations
        else $(logDebug) $ "Transforming source " <> showt s.id <> " with " <> showt req.transformations
      E.catchAny
        do
          s' <- E.throwOnLeft (tryFrom s)
          transformer req.transformations s' >>= E.throwOnLeft <&> UpdatedSource
        \(SomeException e) -> do
          let cause = displayException e
          let msg = if preview then "Transformation preview failed" else "Transformation failed"
          $(logErrorV ['cause]) msg
          pure FailedSourceUpdate {id = s.id, msg = from cause}

data StreamSourceGroups = StreamSourceGroups
  { collectionId :: Ty.CollectionRef,
    groupByMappings :: Vector Text,
    groupFilter :: Maybe SourceGroupFilter
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions StreamSourceGroups

streamSourceGroups :: StreamSourceGroups -> ContT () (AppM IO IO) (S.Stream (S.Of Ty.SourceGroup) (AppM IO IO) ())
streamSourceGroups rq = do
  groupByMappings <- lift $ getMappingsNamed rq.groupByMappings
  stream <- selectStream sources
  pure
    do
      stream
        & groupSources groupByMappings
        & S.mapped mkSrcGrp
  where
    sources = do
      srcs <- orderByUri $ Rel8.each sourceSchema
      case fromMaybe AllSourceGroups rq.groupFilter of
        AllSourceGroups ->
          Rel8.where_ (srcs.collection_id ==. Rel8.lit rq.collectionId)
        Orphaned -> do
          tracks <- Rel8.each trackSchema
          releases <- Rel8.each releaseSchema
          Rel8.where_
            ( srcs.collection_id ==. Rel8.lit rq.collectionId
                &&. tracks.source_id ==. srcs.id
                &&. releases.id ==. tracks.release_id
                &&. Rel8.isNull releases.musicbrainz_group_id
            )
      pure srcs
    mkSrcGrp :: forall x. S.Stream (S.Of (Ty.SourceEntity, Ty.MappedTags)) (AppM IO IO) x -> AppM IO IO (S.Of Ty.SourceGroup x)
    mkSrcGrp ss = do
      S.next ss >>= \case
        Left x -> do
          let !msg = "Source group cannot be empty"
          $(logErrorIO) msg
          error (from msg)
        Right ((firstSourceEntity, groupTags), rest) -> do
          firstSource <- E.throwOnLeft (tryFrom firstSourceEntity)
          let !groupParentUri = getParentUri (showt firstSource.source)
          coverImage <- getCoverImage firstSource
          let (!all :: S.Stream (S.Of Ty.Source) (AppM IO IO) x) = S.cons firstSource $! S.mapM (E.throwOnLeft . tryFrom . fst) rest
          sources' <- Fold.impurely S.foldM Fold.vectorM all
          pure $ sources' & S.mapOf \sources -> Ty.SourceGroup {groupTags, sources, groupParentUri, coverImage}
    getCoverImage :: Ty.Source -> AppM IO IO (Maybe Ty.Image)
    getCoverImage s =
      firstJustM (findCoverImage . takeDirectory) (uriToFilePath s.source) >>= \case
        Just imgPath ->
          pure $
            Just $
              Ty.Image
                { fileName = Just $ T.pack $ takeFileName imgPath,
                  imageType = Just $ Ty.PictureTypeWrapper F.FrontCover
                }
        Nothing -> pure s.cover

groupSources ::
  (Monad m) =>
  TagMappingIndex ->
  S.Stream (S.Of Ty.SourceEntity) m () ->
  S.Stream (S.Stream (S.Of (Ty.SourceEntity, Ty.MappedTags)) m) m ()
groupSources groupByMappings s =
  s
    & S.mapM
      ( \e -> do
          let !mappings = force $ extractMappedTags groupByMappings e
          pure (e, mappings)
      )
    & S.groupBy (\(_, a) (_, b) -> a == b)
  where
    extractMappedTags :: TagMappingIndex -> Ty.SourceEntity -> Ty.MappedTags
    extractMappedTags mappings e = case tryFrom @_ @F.Metadata e of
      Left _ -> V.empty
      Right metadata ->
        V.fromList $
          filter (not . null . (.values)) $
            mappings ^@.. itraversed <&> \(name, mapping) ->
              Ty.MappedTag
                { mappingName = name,
                  values = metadata.tag mapping
                }

data SourceGroupFilter = AllSourceGroups | Orphaned
  deriving (Generic)
  deriving (TextShow) via FromGeneric SourceGroupFilter
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake, ToLower]] SourceGroupFilter

getParentUri :: Text -> Text
getParentUri srcUri = case parseURI (T.unpack srcUri) of
  Just uri -> case uriScheme uri of
    "file:" -> showt $ fileUri $ takeDirectory $ unEscapeString (uriPath uri)
    _ -> srcUri
  Nothing -> srcUri

data DownloadCover = DownloadCover
  { sourceId :: Ty.SourceRef
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions DownloadCover

getCoverImage :: DownloadCover -> RSocket.StreamId -> AppM IO IO RSocket.Payload
getCoverImage DownloadCover {..} streamId =
  findCoverImageIO sourceId >>= \case
    Nothing -> do
      let !msg = "No cover image found for source " <> showt sourceId.unSourceRef
      $(logWarnIO) msg
      throw $ ServiceException msg
    Just (Right path) -> do
      $(logInfoIO) $ "Found cover image " <> showt path <> " for source " <> showt sourceId.unSourceRef
      contents <- liftIO $ L.readFile path
      let !metadata = encodeFilenameMetadata (takeFileName path)
      pure $ buildPayload (RawPayload contents) metadata streamId True
    Just (Left pic) -> do
      $(logInfoIO) $ "Found embedded cover image for source " <> showt sourceId.unSourceRef
      let !picMime = RSocket.RawMimeType (T.encodeUtf8 pic.mimeType)
      pure $ buildPayload (TypedPayload picMime (L.fromStrict pic.pictureData)) (RSocket.CompositeMetadata []) streamId True
  where
    encodeFilenameMetadata fileName =
      RSocket.TypedMetadata (RSocket.MimeTypeId RSocket.ApplicationJson) $
        RSocket.DataPayload $
          JSON.encode (JSON.object [("file_name", toJSON fileName)])
    findCoverImageIO k = withSpan "findCoverImageIO" defaultSpanArguments do
      getSource k >>= \case
        Nothing -> do
          $(logWarn) $ "No source found for ref " <> showt k
          pure Nothing
        Just src -> case uriToFilePath src.source of
          Nothing -> do
            $(logWarn) $ "No local file found for source " <> showt k
            pure Nothing
          Just path -> do
            $(logInfo) $ "Locating cover image for source " <> showt k <> " at path " <> showt path
            let dir = takeDirectory path
            findCoverImage dir >>= \case
              Just imgPath -> pure $ Just $ Right imgPath
              Nothing -> do
                openMetadataFile path >>= \case
                  Left e -> do
                    let cause = displayException e
                    $(logWarnV ['cause]) $ "Could not look for embedded image in file " <> showt path
                    pure Nothing
                  Right mf -> do
                    let coverKey = fromMaybe F.FrontCover (src.cover ^? _Just . #imageType . _Just . coerced)
                    pure $ Left <$> lookup coverKey mf.pictures
