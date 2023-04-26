{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Control.Concurrent.Classy
import Melo.Common.Exception (displayException, throwIO, toException)
import Melo.Common.Exception qualified as E
import Control.Foldl qualified as Fold
import Control.Lens hiding (from, lens, (|>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary.Builder (append, fromByteString, putStringUtf8)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Coerce
import Data.Either.Combinators
import Data.Generics.Labels ()
import Data.Kind
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Morpheus
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Pool
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock
import Data.Typeable
import Data.UUID (fromText, toText)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Hasql.Connection
import Hasql.CursorTransactionIO.TransactionIO
import Hasql.Session
import Hasql.TransactionIO.Sessions
import Melo.Common.Config
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Common.Uuid
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO (selectStream)
import Melo.Format qualified as F
import Melo.GraphQL.Where
import Melo.Library.Release.Aggregate
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo
import Melo.Library.Release.Types
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.FileSystem.Scan
import Melo.Library.Collection.Repo (runCollectionRepositoryPooledIO)
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Release.Aggregate as Release
import Melo.Library.Source.Aggregate
import Melo.Library.Source.MultiTrack
import Melo.Library.Source.Repo
import Melo.Library.Source.Transform qualified as Tr
import Melo.Library.Source.Types qualified as Ty
import Melo.Library.Track.Aggregate
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo as Track
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz as MB
import Melo.Lookup.Covers (Cover(..), CoverService(..))
import Melo.Lookup.Covers qualified as Covers
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.HTTP.Client as Http
import Network.Wai (StreamingBody)
import Network.Wreq.Session (newAPISession)
import Rel8 (JSONBEncoded (..), (&&.), (==.))
import Rel8 qualified
import Streaming.Prelude qualified as S
import System.FilePath as P
import Witch

data SourceEvent
  = SourceAdded (Ty.Source)
  | SourceRemoved (Ty.SourceRef)
  deriving (Show, Eq)

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
    WithOperation o
  ) =>
  SourcesArgs ->
  Resolver o e m (Vector (Source (Resolver o e m)))
resolveSources args = do
  ss <- resolveSourceEntities args.where'
  pure $ fmap enrichSourceEntity ss

resolveSourceGroups ::
  forall m o e.
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
    WithOperation o
  ) =>
  SourceGroupsArgs ->
  Resolver o e m (V.Vector (SourceGroup (Resolver o e m)))
resolveSourceGroups args = do
  groupByMappings <- lift $ getMappingsNamed args.groupByMappings
  srcs <- resolveSourceEntities args.where'
  lift $
    S.each srcs
      & groupSources' groupByMappings
      & Fold.impurely S.foldM_ Fold.vectorM

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
convertSources = fmap (mapLeft from . tryFrom)

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
            [
              ExternalImage
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
        Just track -> Release.getRelease track.release_id >>= \case
          Nothing -> pure []
          Just release -> fmap from <$> searchForCovers release

instance From Cover Image where
  from CoverInfo {..} = ImageSearchResult
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

instance GQLType CollectionSourcesArgs where
  type KIND CollectionSourcesArgs = INPUT

newtype SourcesArgs = SourceArgs
  { where' :: Maybe SourceWhere
  }
  deriving (Generic)

instance GQLType SourcesArgs where
  type KIND SourcesArgs = INPUT

data SourceWhere = SourceWhere
  { id :: Maybe Where,
    sourceUri :: Maybe Where
  }
  deriving (Generic)

instance GQLType SourceWhere where
  type KIND SourceWhere = INPUT

data SourceGroupsArgs = SourceGroupsArgs
  { groupByMappings :: Vector Text,
    where' :: Maybe SourceWhere
  }
  deriving (Generic)

instance GQLType SourceGroupsArgs

newtype CollectionSourceGroupsArgs = CollectionSourceGroupsArgs
  { groupByMappings :: Vector Text
  }
  deriving (Generic)

instance GQLType CollectionSourceGroupsArgs

data Metadata m = Metadata
  { tags :: Vector Ty.TagPair,
    mappedTags :: MappedTagsArgs -> m MappedTags,
    formatId :: Text,
    format :: Text
  }
  deriving (Generic)

instance Typeable m => GQLType (Metadata m)

type MappedTags = Vector MappedTag

data MappedTag = MappedTag
  { mappingName :: Text,
    values :: Vector Text
  }
  deriving (Show, Eq, Ord, Generic)

instance GQLType MappedTag

data MappedTagsArgs = MappedTagsArgs
  { mappings :: Vector Text
  }
  deriving (Generic)

instance GQLType MappedTagsArgs

instance (TagMappingAggregate m, WithOperation o) => From Ty.SourceEntity (Metadata (Resolver o e m)) where
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

resolveMappedTags :: TagMappingAggregate m => Ty.Source -> Vector Text -> m MappedTags
resolveMappedTags src mappingNames =
  forM mappingNames $ \mappingName -> do
    values <- resolveMappingNamed mappingName src
    pure
      MappedTag
        { mappingName,
          values
        }

resolveCollectionSourceGroups ::
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
    WithOperation o
  ) =>
  Ty.CollectionRef ->
  Vector Text ->
  Resolver o e m (Vector (SourceGroup (Resolver o e m)))
resolveCollectionSourceGroups collectionRef groupMappingNames = lift do
  groupByMappings <- getMappingsNamed groupMappingNames
  srcs <- getCollectionSources collectionRef
  S.each srcs
    & groupSources' groupByMappings
    & Fold.impurely S.foldM_ Fold.vectorM

data SourceGroup m = SourceGroup
  { groupTags :: MappedTags,
    groupParentUri :: Text,
    sources :: [Source m],
    coverImage :: CoverImageArgs -> m [Image]
  }
  deriving (Generic)

instance Typeable m => GQLType (SourceGroup m)

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

instance GQLType ImageInfo

instance From Covers.ImageInfo ImageInfo where
  from s = ImageInfo {
    width = s.width,
    height = s.height,
    url = s.url,
    bytes = s.bytes
  }

data SourceContent
  = Folder [SourceContent]
  | ImageContent
      { fileName :: Text,
        downloadUri :: Text
      }
  deriving (Generic)

instance GQLType SourceContent

data SourceGroupStream m = SourceGroupStream
  { sourceGroup :: SourceGroup m
  }
  deriving (Generic)

instance Typeable m => GQLType (SourceGroupStream m)

data SourceGroupStreamArgs = SourceGroupStreamArgs
  { groupByMappings :: Vector Text
  }
  deriving (Generic)

instance GQLType SourceGroupStreamArgs

groupSources' ::
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    WithOperation o,
    Monad n
  ) =>
  TagMappingIndex ->
  S.Stream (S.Of Ty.SourceEntity) n () ->
  S.Stream (S.Of (SourceGroup (Resolver o e m))) n ()
groupSources' groupByMappings s =
  s
    & S.map (\e -> (e, extractMappedTags groupByMappings e))
    & S.groupBy (\(_, a) (_, b) -> a == b)
    & S.mapped mkSrcGroup

extractMappedTags :: TagMappingIndex -> Ty.SourceEntity -> MappedTags
extractMappedTags mappings e = case tryFrom @_ @F.Metadata e of
  Left _ -> V.empty
  Right metadata ->
    V.fromList $
      filter (not . null . (.values)) $
        Map.assocs mappings <&> \(name, mapping) ->
          MappedTag
            { mappingName = name,
              values = metadata.tag mapping
            }

mkSrcGroup ::
  forall m o e x n.
  ( Tr.MonadSourceTransform m,
    UuidGenerator m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    CoverService m,
    ConfigService m,
    WithOperation o,
    Monad n
  ) =>
  S.Stream (S.Of (Ty.SourceEntity, MappedTags)) n x ->
  n (S.Of (SourceGroup (Resolver o e m)) x)
mkSrcGroup s =
  s
    & S.map (_1 %~ enrichSourceEntity)
    & toSourceGroup
  where
    toSourceGroup ::
      S.Stream (S.Of (Source (Resolver o e m), MappedTags)) n x ->
      n (S.Of (SourceGroup (Resolver o e m)) x)
    toSourceGroup ss = do
      ofSources <- S.toList ss
      pure $ S.mapOf toSourceGroup' ofSources
      where
        toSourceGroup' :: [(Source (Resolver o e m), MappedTags)] -> SourceGroup (Resolver o e m)
        toSourceGroup' ss =
          SourceGroup
            { groupParentUri,
              coverImage,
              groupTags = ss ^? traverse . _2 & fromMaybe V.empty,
              sources
            }
          where
            sources = fst <$> ss
            src = head sources
            groupParentUri = getParentUri src.sourceUri
            coverImage = src.coverImage

data SourceGroupFilter = AllSourceGroups | Orphaned

streamSourceGroupsQuery :: CollectionWatchState -> Pool Connection -> Http.Manager -> Ty.CollectionRef -> TagMappingIndex -> SourceGroupFilter -> L.ByteString -> StreamingBody
streamSourceGroupsQuery collectionWatchState pool httpManager collectionRef groupByMappings filt rq sendChunk flush =
  withResource pool (streamSession >=> either throwIO pure)
  where
    mkRoot :: SourceGroup (Resolver QUERY () m) -> RootResolver m () SourceGroupStream Undefined Undefined
    mkRoot sourceGroup =
      defaultRootResolver
        { queryResolver =
            SourceGroupStream
              { sourceGroup
              }
        }
    sourcesForCollection collectionRef = do
      srcs <- orderByUri $ Rel8.each sourceSchema
      case filt of
        AllSourceGroups ->
          Rel8.where_ (srcs.collection_id ==. Rel8.lit collectionRef)
        Orphaned -> do
          tracks <- Rel8.each trackSchema
          releases <- Rel8.each releaseSchema
          Rel8.where_
            ( srcs.collection_id ==. Rel8.lit collectionRef
                &&. tracks.source_id ==. srcs.id
                &&. releases.id ==. tracks.release_id
                &&. Rel8.isNull releases.musicbrainz_group_id
            )
      pure srcs
    streamSession :: Connection -> IO (Either QueryError ())
    streamSession conn = do
      let q = sourcesForCollection collectionRef
      $(logInfoIO) $ show (Rel8.showQuery q)
      flip run conn $
        transactionIO ReadCommitted ReadOnly NotDeferrable $
          cursorTransactionIO $
            processStream (selectStream q)
    processStream :: MonadIO m => S.Stream (S.Of Ty.SourceEntity) m () -> m ()
    processStream s = do
      $(logInfoIO) $ "Starting streaming sources from collection " <> show collectionRef
      sess <- liftIO newAPISession
      let runIO = liftIO . runSourceIO collectionWatchState sess pool httpManager
      s
        & groupSources' groupByMappings
        & S.mapM (\srcGrp -> runIO $ interpreter (mkRoot srcGrp) (L.toStrict rq))
        & S.mapM_ sendFlush
      $(logInfoIO) $ "Finished streaming sources from collection " <> show collectionRef
    sendFlush :: MonadIO m => BS.ByteString -> m ()
    sendFlush = (liftIO . (const flush)) <=< liftIO . sendChunk . (`append` (putStringUtf8 "\n\n")) . fromByteString

runSourceIO collectionWatchState sess pool httpManager =
  runFileSystemIO
    . runConfigRepositoryPooledIO pool
    . runFileSystemWatcherIO pool collectionWatchState sess
    . runMetadataAggregateIO
    . runSourceRepositoryPooledIO pool
    . runTagMappingRepositoryPooledIO pool
    . runReleaseRepositoryPooledIO pool
    . runReleaseArtistNameRepositoryPooledIO pool
    . runArtistNameRepositoryPooledIO pool
    . runArtistRepositoryPooledIO pool
    . runTrackArtistNameRepositoryPooledIO pool
    . runTrackRepositoryPooledIO pool
    . runMusicBrainzServiceUnlimitedIO sess
    . runMultiTrackIO
    . runCollectionRepositoryPooledIO pool
    . runCollectionAggregateIO pool sess collectionWatchState
    . runTagMappingAggregate
    . runArtistAggregateIOT
    . runTrackAggregateIOT
    . runReleaseAggregateIOT
    . runSourceAggregateIOT
    . Covers.runCoverServiceIO httpManager

getParentUri :: Text -> Text
getParentUri srcUri = case parseURI (T.unpack srcUri) of
  Just uri -> case uriScheme uri of
    "file:" -> T.pack $ show $ fileUri $ takeDirectory $ unEscapeString (uriPath uri)
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
  deriving (Generic)

instance GQLType TransformSourceArgs where
  type KIND TransformSourceArgs = INPUT

type TransformSources m = TransformSourcesArgs -> m (UpdatedSources m)

data TransformSourcesArgs = TransformSourcesArgs
  { transformations :: Vector Transform,
    where' :: Maybe SourceWhere
  }
  deriving (Generic)

instance GQLType TransformSourcesArgs where
  type KIND TransformSourcesArgs = INPUT

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
  deriving (Show, Generic)

instance GQLType Transform where
  type KIND Transform = INPUT

data MetadataTransformation
  = SetMapping {mapping :: Text, values :: Vector Text}
  | RemoveMappings {mappings :: Vector Text}
  | RetainMappings {mappings :: Vector Text}
  | AddTag {key :: Text, value :: Text}
  | RemoveTag {key :: Text, value :: Text}
  | RemoveTags {key :: Text}
  | RemoveAll
  deriving (Show, Generic)

instance GQLType MetadataTransformation where
  type KIND MetadataTransformation = INPUT

previewTransformSourceImpl ::
  forall m e o.
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    WithOperation o
  ) =>
  Ty.SourceEntity ->
  Vector Transform ->
  m (UpdateSourceResult (Resolver o e m))
previewTransformSourceImpl s ts = do
  $(logDebug) $ "Preview; Transforming source " <> show s.id <> " with " <> show ts
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
        $(logError) $ E.displayException e
        pure $ FailedSourceUpdate s.id (T.pack $ E.displayException e)
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
    Tr.MonadSourceTransform m
  ) =>
  TransformSourcesArgs ->
  ResolverM e m (UpdatedSources (Resolver MUTATION e m))
transformSourcesImpl (TransformSourcesArgs ts where') = do
  es <- resolveSourceEntities where'
  $(logDebug) $ "Transforming sources " <> show (es <&> \e -> e.id) <> " with " <> show ts
  ss <- lift $ forM es $ \s ->
    case tryFrom s of
      Left e -> pure $ Left (s.id, into @Tr.TransformationError e)
      Right s' -> mapLeft (s.id,) <$> Tr.evalTransformActions (interpretTransforms ts) s'
  lift $ fork $ void $ importReleases (V.mapMaybe rightToMaybe ss)
  forM ss \case
    Left (id, e) -> do
      $(logError) $ E.displayException e
      pure $ FailedSourceUpdate id (T.pack $ E.displayException e)
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
    where
      parseMovePattern' pat = mapLeft (TryFromException t <$> fmap toException) $ Tr.parseMovePattern pat
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
