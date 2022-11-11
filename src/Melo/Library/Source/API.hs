{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Control.Concurrent.Classy
import Control.Exception.Safe (displayException, throwIO, toException)
import Control.Exception.Safe qualified as E
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
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.IO (selectStream)
import Melo.Format qualified as F
import Melo.GraphQL.Where
import Melo.Library.Album.Aggregate
import Melo.Library.Album.ArtistName.Repo
import Melo.Library.Album.Repo
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.FileSystem.Watcher
import Melo.Library.Collection.Repo (runCollectionRepositoryPooledIO)
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Source.Aggregate
import Melo.Library.Source.MultiTrack
import Melo.Library.Source.Repo
import Melo.Library.Source.Transform qualified as Tr
import Melo.Library.Source.Types qualified as Ty
import Melo.Library.Track.Aggregate
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz as MB
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.Wai (StreamingBody)
import Network.Wreq.Session (newAPISession)
import Rel8 (JSONBEncoded (..), (==.))
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
    MonadConc m,
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
    MonadConc m,
    WithOperation o
  ) =>
  SourceGroupsArgs ->
  Resolver o e m (V.Vector (SourceGroup (Resolver o e m)))
resolveSourceGroups args = do
  groupByMappings <- lift $ getMappingsNamed args.groupByMappings
  srcs <- resolveSourceEntities args.where'
  lift $ S.each srcs
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
    MonadConc m,
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
    coverImage :: m (Maybe Image),
    previewTransform :: TransformSource m
  }
  deriving (Generic)

instance Typeable m => GQLType (Source m)

enrichSourceEntity ::
  forall m o e.
  ( Tr.MonadSourceTransform m,
    MonadConc m,
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
          coverImage = lift $ coverImageImpl s filePath,
          previewTransform = \args -> lift $ previewTransformSourceImpl s args.transformations
        }
  where
    sourceLengthImpl :: Ty.SourceEntity -> m (Maybe Double)
    sourceLengthImpl Ty.SourceTable {time_range = (Just intervalRange)} =
      pure $
        realToFrac . (* 1000) . nominalDiffTimeToSeconds <$> Ty.rangeLength intervalRange
    sourceLengthImpl _ = pure Nothing
    coverImageImpl :: Ty.SourceEntity -> Maybe FilePath -> m (Maybe Image)
    coverImageImpl _ Nothing = pure Nothing
    coverImageImpl src (Just path) =
      let downloadUri = "/source/" <> toText (Ty.unSourceRef src.id) <> "/image"
       in findCoverImage (takeDirectory path) >>= \case
            Just imgPath ->
              pure $
                Just
                  ExternalImage
                    { fileName = T.pack $ takeFileName imgPath,
                      downloadUri
                    }
            Nothing ->
              pure (EmbeddedImage <$> coerce src.cover <*> pure downloadUri)

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
        format = either (const "") (\m' -> m'.formatDesc) m
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
    MonadConc m,
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
    coverImage :: m (Maybe Image)
  }
  deriving (Generic)

instance Typeable m => GQLType (SourceGroup m)

data Image
  = ExternalImage
      { fileName :: Text,
        downloadUri :: Text
      }
  | EmbeddedImage
      { imageType :: Ty.PictureTypeWrapper,
        downloadUri :: Text
      }
  deriving (Generic)

instance GQLType Image

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
  (Tr.MonadSourceTransform m, WithOperation o, Monad n) =>
  TagMappingIndex ->
  S.Stream (S.Of Ty.SourceEntity) n () ->
  S.Stream (S.Of (SourceGroup (Resolver o e m))) n ()
groupSources' groupByMappings s = s
  & S.map (\e -> (e, extractMappedTags groupByMappings e))
  & S.groupBy (\(_,a) (_,b) -> a == b)
  & S.mapped mkSrcGroup

extractMappedTags :: TagMappingIndex -> Ty.SourceEntity -> MappedTags
extractMappedTags mappings e = case tryFrom @_ @F.Metadata e of
  Left _ -> V.empty
  Right metadata ->
    V.fromList $ filter (not . null . (.values)) $ Map.assocs mappings <&> \(name, mapping) -> MappedTag {
      mappingName = name,
      values = metadata.tag mapping
    }

mkSrcGroup :: forall m o e x n. (Tr.MonadSourceTransform m, WithOperation o, Monad n) =>
  S.Stream (S.Of (Ty.SourceEntity, MappedTags)) n x -> n (S.Of (SourceGroup (Resolver o e m)) x)
mkSrcGroup s = s
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
          coverImage = case parseURI (T.unpack groupParentUri) >>= uriToFilePath of
            Just dir ->
              lift (findCoverImage dir) >>= \case
                Nothing -> src.coverImage
                Just imgPath ->
                  pure $
                    Just
                      ExternalImage
                        { fileName = T.pack $ takeFileName imgPath,
                          downloadUri = "/source/" <> toText (coerce (src.id)) <> "/image"
                        }
            Nothing -> pure Nothing

streamSourceGroupsQuery :: CollectionWatchState -> Pool Connection -> Ty.CollectionRef -> TagMappingIndex -> L.ByteString -> StreamingBody
streamSourceGroupsQuery collectionWatchState pool collectionRef groupByMappings rq sendChunk flush =
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
      Rel8.where_ (srcs.collection_id ==. Rel8.lit collectionRef)
      pure srcs
    streamSession :: Connection -> IO (Either QueryError ())
    streamSession =
      run $
        transactionIO ReadCommitted ReadOnly NotDeferrable $
          cursorTransactionIO $
            processStream (selectStream (sourcesForCollection collectionRef))
    processStream :: MonadIO m => S.Stream (S.Of Ty.SourceEntity) m () -> m ()
    processStream s = do
      $(logInfoIO) $ "Starting streaming sources from collection " <> show collectionRef
      sess <- liftIO newAPISession
      let runIO = liftIO . runSourceIO collectionWatchState sess pool
      s
        & groupSources' groupByMappings
        & S.mapM (\srcGrp -> runIO $ interpreter (mkRoot srcGrp) (L.toStrict rq))
        & S.mapM_ sendFlush
      $(logInfoIO) $ "Finished streaming sources from collection " <> show collectionRef
    sendFlush :: MonadIO m => BS.ByteString -> m ()
    sendFlush = (liftIO . (const flush)) <=< liftIO . sendChunk . (`append` (putStringUtf8 "\n\n")) . fromByteString

runSourceIO collectionWatchState sess pool =
  runFileSystemIO
    . runMetadataAggregateIO
    . runSourceRepositoryPooledIO pool
    . runTagMappingRepositoryPooledIO pool
    . runAlbumRepositoryPooledIO pool
    . runAlbumArtistNameRepositoryPooledIO pool
    . runArtistNameRepositoryPooledIO pool
    . runArtistRepositoryPooledIO pool
    . runTrackArtistNameRepositoryPooledIO pool
    . runTrackRepositoryPooledIO pool
    . runMusicBrainzServiceUnlimitedIO sess
    . runFileSystemWatcherIO pool collectionWatchState sess
    . runMultiTrackIO
    . runCollectionRepositoryPooledIO pool
    . runCollectionAggregateIO pool sess
    . runArtistAggregateIOT
    . runTrackAggregateIOT
    . runAlbumAggregateIOT
    . runSourceAggregateIOT
    . runTagMappingAggregate

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
  deriving (Show, Generic)

instance GQLType Transform where
  type KIND Transform = INPUT

data MetadataTransformation
  = SetMapping {mapping :: Text, values :: Vector Text}
  | RemoveMappings {mappings :: Vector Text}
  | Retain {mappings :: Vector Text}
  deriving (Show, Generic)

instance GQLType MetadataTransformation where
  type KIND MetadataTransformation = INPUT

previewTransformSourceImpl ::
  forall m e o.
  ( Tr.MonadSourceTransform m,
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
        Right s' -> do
          Tr.previewTransformation (Tr.evalTransformActions (interpretTransforms ts)) s' >>= \case
            Left e -> E.throwM e
            Right s'' -> pure $ UpdatedSource (enrichSourceEntity (from s''))
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
    Tr.MonadSourceTransform m
  ) =>
  TransformSourcesArgs ->
  ResolverM e m (UpdatedSources (Resolver MUTATION e m))
transformSourcesImpl (TransformSourcesArgs ts where') = do
  es <- resolveSourceEntities where'
  $(logDebug) $ "Transforming sources " <> show (es <&> \e -> e.id) <> " with " <> show ts
  forM es $ \s ->
    lift $
      E.catchAny
        ( case tryFrom s of
            Left e -> E.throwM (into @Tr.TransformationError e)
            Right s' -> do
              Tr.evalTransformActions (interpretTransforms ts) s' >>= \case
                Left e -> E.throwM e
                Right s'' -> pure $ UpdatedSource (enrichSourceEntity @m @MUTATION (from s''))
        )
        ( \e -> do
            $(logError) $ E.displayException e
            pure $ FailedSourceUpdate s.id (T.pack $ E.displayException e)
        )
  where
    interpretTransforms :: Vector Transform -> Vector Tr.TransformAction
    interpretTransforms ts = V.mapMaybe (rightToMaybe . tryFrom) ts

instance TryFrom Transform Tr.TransformAction where
  tryFrom t = case t of
    Move {} -> Tr.Move <$> parseRef t.collectionRef <*> parseMovePattern' t.destPattern
    SplitMultiTrackFile {} -> Tr.SplitMultiTrackFile <$> parseRef t.collectionRef <*> parseMovePattern' t.destPattern
    EditMetadata mt -> Right $ Tr.EditMetadata (from mt)
    where
      parseMovePattern' pat = mapLeft (TryFromException t <$> fmap toException) $ Tr.parseMovePattern pat
      parseRef Nothing = Right Nothing
      parseRef (Just ref) = mapRight Just $ maybeToRight (TryFromException t Nothing) $ Ty.CollectionRef <$> fromText ref

instance From MetadataTransformation Tr.MetadataTransformation where
  from (SetMapping m vs) = Tr.SetMapping m vs
  from (RemoveMappings ms) = Tr.RemoveMappings ms
  from (Retain ms) = Tr.Retain ms
