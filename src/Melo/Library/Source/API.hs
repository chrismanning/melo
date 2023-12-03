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
import Data.Aeson (ToJSON(..))
import Data.Aeson qualified as JSON
import Data.ByteString.Lazy qualified as L
import Data.Coerce
import Data.Either.Combinators
import Data.Generics.Labels ()
import Data.Kind
import Data.Morpheus.Types
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
import Melo.GraphQL.Where
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
import Rel8 (JSONBEncoded (..), (&&.), (==.))
import Rel8 qualified
import Streaming qualified as S
import Streaming.Prelude qualified as S
import System.FilePath as P
import Prelude hiding (log)

resolveSources ::
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    WithOperation o
  ) =>
  SourcesArgs ->
  Resolver o e m (Vector (Source (Resolver o e m)))
resolveSources args = do
  ss <- resolveSourceEntities args.where'
  pure $ fmap enrichSourceEntity ss

resolveSourceEntities ::
  (Tr.MonadSourceTransform m, WithOperation o) =>
  Maybe SourceWhere ->
  Resolver o e m (Vector Ty.SourceEntity)
resolveSourceEntities (Just SourceWhere {..}) =
  case id of
    Just idExpr -> case idExpr of
      WhereEqExpr (EqExpr x) -> case fromText x of
        Just uuid -> lift $ getByKey (V.singleton (Ty.SourceRef uuid))
        Nothing -> fail $ "invalid source id " <> show x
      WhereInExpr (InExpr x) -> case allJust (fmap fromText x) of
        Just uuids -> lift $ getByKey (V.fromList $ Ty.SourceRef <$> uuids)
        Nothing -> fail $ "invalid source id in " <> show x
      _unknownWhere -> fail "invalid where clause for Source.id"
    Nothing -> case sourceUri of
      Just sourceUriExpr -> case sourceUriExpr of
        WhereEqExpr (EqExpr x) -> case parseURI (T.unpack x) of
          Just uri -> lift $ getByUri (V.singleton uri)
          Nothing -> fail $ "invalid source uri " <> show x
        WhereInExpr (InExpr x) -> case allJust (fmap (parseURI . T.unpack) x) of
          Just uris -> lift $ getByUri (V.fromList uris)
          Nothing -> fail $ "invalid source uri in " <> show x
        WhereStartsWithExpr (StartsWithExpr x) -> case parseURI $ T.unpack x of
          Just uri -> lift $ getByUriPrefix uri
          Nothing -> fail $ "invalid source uri in " <> show x
        _unknownWhere -> fail "invalid where clause for Source.sourceUri"
      Nothing -> lift getAll
  where
    allJust :: [Maybe a] -> Maybe [a]
    allJust [] = Just []
    allJust (Just a : as) = fmap (a :) (allJust as)
    allJust (Nothing : _) = Nothing
resolveSourceEntities _ = lift getAll

convertSources :: Vector Ty.SourceEntity -> Vector (Either Tr.TransformationError Ty.Source)
convertSources = fmap (first from . tryFrom)

resolveCollectionSources ::
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    WithOperation o
  ) =>
  Ty.CollectionRef ->
  CollectionSourcesArgs ->
  Resolver o e m (Vector (Source (Resolver o e m)))
resolveCollectionSources collectionRef _args =
  -- TODO handle args
  lift $ getCollectionSources collectionRef <&> fmap enrichSourceEntity

data Source (m :: Type -> Type) = Source
  { id :: Ty.SourceRef,
    format :: Text,
    metadata :: Metadata m,
    sourceName :: Text,
    sourceUri :: Text,
    filePath :: Maybe Text,
    downloadUri :: Text,
    length :: m (Maybe Double),
    coverImage :: CoverImageArgs -> m [Image],
    previewTransform :: TransformSource m
  }
  deriving (Generic)

instance Typeable m => GQLType (Source m)

enrichSourceEntity ::
  forall m o e.
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    WithOperation o
  ) =>
  Ty.SourceEntity ->
  Source (Resolver o e m)
enrichSourceEntity s =
  let filePath = parseURI (T.unpack s.source_uri) >>= uriToFilePath
   in Source
        { id = s.id,
          format = s.kind,
          metadata = from s,
          sourceName = fromMaybe s.source_uri $ T.pack . takeFileName <$> filePath,
          sourceUri = s.source_uri,
          filePath = T.pack <$> filePath,
          downloadUri = "/source/" <> toText (Ty.unSourceRef s.id),
          length = lift $ sourceLengthImpl s,
          coverImage = \args -> lift $ coverImageImpl s args filePath,
          previewTransform = \args -> lift $ previewTransformSourceImpl s args.transformations
        }
  where
    sourceLengthImpl :: Ty.SourceEntity -> m (Maybe Double)
    sourceLengthImpl Ty.SourceTable {time_range = (Just intervalRange)} =
      pure $
        realToFrac . (* 1000) . nominalDiffTimeToSeconds <$> Ty.rangeLength intervalRange
    sourceLengthImpl _ = pure Nothing
    coverImageImpl :: Ty.SourceEntity -> CoverImageArgs -> Maybe FilePath -> m [Image]
    coverImageImpl _ (CoverImageArgs Nothing) path = coverImageFile path
    coverImageImpl _ (CoverImageArgs (Just False)) path = coverImageFile path
    coverImageImpl _ (CoverImageArgs (Just True)) path = (<>) <$> coverImageFile path <*> coverSearch
    coverImageFile Nothing = pure []
    coverImageFile (Just path) =
      findCoverImage (takeDirectory path) >>= \case
        Just imgPath ->
          pure $
            [ ExternalImage
                { fileName = T.pack $ takeFileName imgPath,
                  downloadUri
                }
            ]
        Nothing ->
          pure $ catMaybes [EmbeddedImage <$> coerce s.cover <*> pure downloadUri]
    downloadUri = "/source/" <> toText (Ty.unSourceRef s.id) <> "/image"
    coverSearch =
      Track.getBySrcRef s.id >>= \case
        Nothing -> pure []
        Just track ->
          Release.getRelease track.release_id >>= \case
            Nothing -> pure []
            Just release -> fmap from <$> searchForCovers release

instance From Cover Image where
  from CoverInfo {..} =
    ImageSearchResult
      { bigCover = from bigCover,
        smallCover = from smallCover,
        source = from source
      }

instance From Covers.CoverSource CoverSource where
  from Covers.Bandcamp = Bandcamp
  from Covers.Qobuz = Qobuz
  from Covers.Tidal = Tidal

newtype CollectionSourcesArgs = CollectionSourcesArgs
  { where' :: Maybe SourceWhere
  }
  deriving (Generic)
  deriving anyclass (GQLType)

newtype SourcesArgs = SourceArgs
  { where' :: Maybe SourceWhere
  }
  deriving (Generic)
  deriving anyclass (GQLType)

data SourceWhere = SourceWhere
  { id :: Maybe Where,
    sourceUri :: Maybe Where
  }
  deriving (Generic, GQLType)

data SourceGroupsArgs = SourceGroupsArgs
  { groupByMappings :: Vector Text,
    where' :: Maybe SourceWhere
  }
  deriving (Generic, GQLType)

newtype CollectionSourceGroupsArgs = CollectionSourceGroupsArgs
  { groupByMappings :: Vector Text
  }
  deriving (Generic)
  deriving anyclass (GQLType)

data Metadata m = Metadata
  { tags :: Vector Ty.TagPair,
    mappedTags :: MappedTagsArgs -> m Ty.MappedTags,
    formatId :: Text,
    format :: Text
  }
  deriving (Generic)

instance Typeable m => GQLType (Metadata m)

deriving instance GQLType Ty.MappedTag
deriving instance NFData Ty.MappedTag

data MappedTagsArgs = MappedTagsArgs
  { mappings :: Vector Text
  }
  deriving (Generic, GQLType)

instance
  ( TagMappingAggregate m,
    Tracing m,
    WithOperation o
  ) =>
  From Ty.SourceEntity (Metadata (Resolver o e m))
  where
  from s =
    let s' = tryFrom @_ @Ty.Source s
        m = fmap (.metadata) s'
        JSONBEncoded (Ty.SourceMetadata tags) = s.metadata
        format = fromMaybe "" $ either (const $ Just "") (fmap (.formatDesc)) m
     in Metadata
          { tags,
            format,
            formatId = s.metadata_format,
            mappedTags = \args -> case s' of
              Right src -> lift $ resolveMappedTags src args.mappings
              Left e -> fail $ "failed to convert SourceEntity: " <> displayException e
          }

resolveMappedTags :: (TagMappingAggregate m, Tracing m) => Ty.Source -> Vector Text -> m Ty.MappedTags
resolveMappedTags src mappingNames = withSpan "resolveMappedTags" defaultSpanArguments do
  forM mappingNames $ \mappingName -> do
    values <- resolveMappingNamed mappingName src
    pure
      Ty.MappedTag
        { mappingName,
          values
        }

data CoverImageArgs = CoverImageArgs
  { search :: Maybe Bool
  }
  deriving (Generic)

instance GQLType CoverImageArgs

data CoverSource
  = FileSystem
  | Bandcamp
  | Qobuz
  | Tidal
  deriving (Generic)

instance GQLType CoverSource

data Image
  = ExternalImage
      { fileName :: Text,
        downloadUri :: Text
      }
  | EmbeddedImage
      { imageType :: Ty.PictureTypeWrapper,
        downloadUri :: Text
      }
  | ImageSearchResult
      { smallCover :: ImageInfo,
        bigCover :: ImageInfo,
        source :: CoverSource
      }
  deriving (Generic)

instance GQLType Image

data ImageInfo = ImageInfo
  { width :: Int,
    height :: Int,
    url :: Text,
    bytes :: Int
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via FromGeneric ImageInfo

instance GQLType ImageInfo

instance From Covers.ImageInfo ImageInfo where
  from s =
    ImageInfo
      { width = s.width,
        height = s.height,
        url = s.url,
        bytes = s.bytes
      }

groupSources ::
  ( Monad m,
    Logging m
  ) =>
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
  deriving TextShow via FromGeneric SourceGroupFilter
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake, ToLower]] SourceGroupFilter

getParentUri :: Text -> Text
getParentUri srcUri = case parseURI (T.unpack srcUri) of
  Just uri -> case uriScheme uri of
    "file:" -> showt $ fileUri $ takeDirectory $ unEscapeString (uriPath uri)
    _ -> srcUri
  Nothing -> srcUri

type UpdatedSources m = Vector (UpdateSourceResult m)

data UpdateSourceResult m
  = UpdatedSource (Source m)
  | FailedSourceUpdate {id :: Ty.SourceRef, msg :: Text}
  deriving (Generic)

instance Typeable m => GQLType (UpdateSourceResult m)

type TransformSource m = TransformSourceArgs -> m (UpdateSourceResult m)

data TransformSourceArgs = TransformSourceArgs
  { transformations :: Vector Transform
  }
  deriving (Generic, GQLType)

type TransformSources m = TransformSourcesArgs -> m (UpdatedSources m)

data TransformSourcesArgs = TransformSourcesArgs
  { transformations :: Vector Transform,
    where' :: Maybe SourceWhere
  }
  deriving (Generic, GQLType)

data Transform
  = Move
      { destPattern :: Text,
        collectionRef :: Maybe Text
      }
  | SplitMultiTrackFile
      { destPattern :: Text,
        collectionRef :: Maybe Text
      }
  | EditMetadata {metadataTransform :: MetadataTransformation}
  | MusicBrainzLookup {options :: Maybe Int}
  | CopyCoverImage {url :: Text}
  | ConvertMetadataFormat {targetId :: Text}
  deriving (Show, Generic, GQLType)
  deriving (TextShow) via FromGeneric Transform

data MetadataTransformation
  = SetMapping {mapping :: Text, values :: Vector Text}
  | RemoveMappings {mappings :: Vector Text}
  | RetainMappings {mappings :: Vector Text}
  | AddTag {key :: Text, value :: Text}
  | RemoveTag {key :: Text, value :: Text}
  | RemoveTags {key :: Text}
  | RemoveAll
  deriving (Show, Generic, GQLType)
  deriving (TextShow) via FromGeneric MetadataTransformation

previewTransformSourceImpl ::
  forall m e o.
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    CollectionRepository m,
    TrackRepository m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    E.MonadCatch m,
    Tracing m,
    Typeable m,
    WithOperation o
  ) =>
  Ty.SourceEntity ->
  Vector Transform ->
  m (UpdateSourceResult (Resolver o e m))
previewTransformSourceImpl s ts = withSpan "previewTransformSourceImpl" defaultSpanArguments do
  $(logDebug) $ "Preview; Transforming source " <> showt s.id <> " with " <> showt ts
  E.catchAny
    ( case tryFrom s of
        Left e -> E.throwM (into @Tr.TransformationError e)
        Right s' ->
          Tr.previewTransformation (Tr.evalTransformActions (interpretTransforms ts)) s' >>= \case
            Left e -> E.throwM e
            Right s'' -> do
              pure $ UpdatedSource (enrichSourceEntity (from s''))
    )
    ( \e -> do
        let cause = displayException e
        $(logErrorV ['cause]) "Preview; Transform failed"
        pure $ FailedSourceUpdate s.id (from $ E.displayException e)
    )
  where
    interpretTransforms :: Vector Transform -> Vector Tr.TransformAction
    interpretTransforms ts = V.mapMaybe (rightToMaybe . tryFrom) ts

transformSourcesImpl ::
  forall m e.
  ( MonadConc m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    Tr.MonadSourceTransform m
  ) =>
  TransformSourcesArgs ->
  ResolverM e m (UpdatedSources (Resolver MUTATION e m))
transformSourcesImpl (TransformSourcesArgs ts where') = do
  es <- resolveSourceEntities where'
  $(logDebug) $ "Transforming sources " <> showt (es <&> \e -> e.id) <> " with " <> showt ts
  ss <- lift $ forM es $ \s ->
    case tryFrom s of
      Left e -> pure $ Left (s.id, into @Tr.TransformationError e)
      Right s' -> first (s.id,) <$> Tr.evalTransformActions (interpretTransforms ts) s'
  lift $ fork $ void $ importReleases (V.mapMaybe rightToMaybe ss)
  forM ss \case
    Left (id, e) -> do
      let cause = displayException e
      $(logErrorV ['cause]) "Transform failed"
      pure $ FailedSourceUpdate id (from $ E.displayException e)
    Right s -> pure $ UpdatedSource (enrichSourceEntity @m @MUTATION (from s))
  where
    interpretTransforms :: Vector Transform -> Vector Tr.TransformAction
    interpretTransforms ts = V.mapMaybe (rightToMaybe . tryFrom) ts

instance TryFrom Transform Tr.TransformAction where
  tryFrom t = case t of
    Move {} -> Tr.Move <$> parseRef t.collectionRef <*> parseMovePattern' t.destPattern
    SplitMultiTrackFile {} -> Tr.SplitMultiTrackFile <$> parseRef t.collectionRef <*> parseMovePattern' t.destPattern
    EditMetadata mt -> Right $ Tr.EditMetadata (V.singleton (from mt))
    MusicBrainzLookup _ -> Right Tr.MusicBrainzLookup
    CopyCoverImage url -> maybeToRight (TryFromException t Nothing) $ Tr.CopyCoverImage <$> parseURI (T.unpack url)
    ConvertMetadataFormat mid -> Right $ Tr.ConvertMetadataFormat (F.MetadataId mid)
    where
      parseMovePattern' pat = first (TryFromException t <$> fmap toException) $ Tr.parseMovePattern pat
      parseRef Nothing = Right Nothing
      parseRef (Just ref) = mapRight Just $ maybeToRight (TryFromException t Nothing) $ Ty.CollectionRef <$> fromText ref

instance From MetadataTransformation Tr.MetadataTransformation where
  from (SetMapping m vs) = Tr.SetMapping m vs
  from (RemoveMappings ms) = Tr.RemoveMappings ms
  from (RetainMappings ms) = Tr.RetainMappings ms
  from (AddTag k v) = Tr.AddTag k v
  from (RemoveTag k v) = Tr.RemoveTag k v
  from (RemoveTags k) = Tr.RemoveTags k
  from RemoveAll = Tr.RemoveAll

-- RSocket API

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "downloadCover") (jsonRqRsRoute getCoverImage)
  registerRoute (RouteKey "getSources") (jsonStreamRoute streamSourceGroups)
  pure ()

data StreamSourceGroups = StreamSourceGroups
  { collectionId :: Ty.CollectionRef
  , groupByMappings :: Vector Text
  , groupFilter :: Maybe SourceGroupFilter
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions StreamSourceGroups

streamSourceGroups :: StreamSourceGroups -> RSocket.StreamId -> ContT () (AppM IO IO) (S.Stream (S.Of RSocket.Payload) (AppM IO IO) ())
streamSourceGroups rq streamId = do
  groupByMappings <- lift $ getMappingsNamed rq.groupByMappings
  stream <- selectStream sources
  pure
    do
      stream
        & groupSources groupByMappings
        & S.mapped (\srcs -> mkSrcGrp srcs)
        & S.map (\(srcGrp :: Ty.SourceGroup) -> buildStreamPayload (JsonPayload srcGrp) (RSocket.CompositeMetadata []) streamId)
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
          pure $ sources' & S.mapOf \sources -> Ty.SourceGroup { groupTags, sources, groupParentUri, coverImage }
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
