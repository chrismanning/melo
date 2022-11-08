{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Control.Applicative hiding (many)
import Control.Concurrent.Classy
import Control.Exception.Safe (throwIO, toException)
import Control.Exception.Safe qualified as E
import Control.Lens hiding (from, lens, (|>))
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary.Builder (append, fromByteString, putStringUtf8)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Coerce
import Data.Default
import Data.Either.Combinators
import Data.Foldable
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as H
import Data.Kind
import Data.List (sortBy)
import Data.Maybe
import Data.Morpheus
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Pool
import Data.Sequence ((|>))
import Data.Sequence qualified as S
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
import Melo.Common.NaturalSort
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.IO (selectStream)
import Melo.Format ()
import Melo.Format qualified as F
import Melo.Format.Mapping qualified as M
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
import Melo.Metadata.Mapping.Repo
import Network.Wai (StreamingBody)
import Network.Wreq.Session (newAPISession)
import Rel8 (JSONBEncoded (..), (==.))
import Rel8 qualified
import Streaming.Prelude qualified as S
import System.FilePath as P
import Witch

resolveSources ::
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    WithOperation o
  ) =>
  SourcesArgs ->
  Resolver o e m (Vector (Source (Resolver o e m)))
resolveSources args = do
  ss <- resolveSourceEntities args
  pure $ fmap enrichSourceEntity ss

resolveSourceEntities ::
  (Tr.MonadSourceTransform m, WithOperation o) =>
  SourcesArgs ->
  Resolver o e m (Vector Ty.SourceEntity)
resolveSourceEntities (SourceArgs (Just SourceWhere {..})) =
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
    metadata :: Metadata,
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
          length = lift $ sourceLengthImpl s filePath,
          coverImage = lift $ coverImageImpl s filePath,
          previewTransform = \args -> lift $ previewTransformSourceImpl s args.transformations
        }
  where
    sourceLengthImpl :: Ty.SourceEntity -> Maybe FilePath -> m (Maybe Double)
    sourceLengthImpl Ty.SourceTable {time_range = (Just intervalRange)} _ =
      pure $
        realToFrac . (* 1000) . nominalDiffTimeToSeconds <$> Ty.rangeLength intervalRange
    sourceLengthImpl _ (Just p) =
      openMetadataFile p >>= \case
        Left e -> do
          $(logWarn) $ "failed to open file " <> p <> ": " <> E.displayException e
          pure Nothing
        Right mf -> pure $ F.audioLengthMilliseconds mf.audioInfo
    sourceLengthImpl _ _ = pure Nothing
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

data TimeUnit = Seconds | Milliseconds | Nanoseconds
  deriving (Generic)

data Metadata = Metadata
  { tags :: V.Vector Ty.TagPair,
    mappedTags :: MappedTags,
    formatId :: Text,
    format :: Text
  }
  deriving (Generic)

instance GQLType Metadata

instance From F.Metadata Metadata where
  from m =
    Metadata
      { formatId = coerce m.formatId,
        format = m.formatDesc,
        tags = uncurry Ty.TagPair <$> coerce m.tags,
        mappedTags = mapTags m
      }

instance From Ty.SourceEntity Metadata where
  from s =
    let s' = tryFrom @_ @Ty.Source s
        m = fmap (\s'' -> s''.metadata) s'
        JSONBEncoded (Ty.SourceMetadata tags) = s.metadata
        format = either (const "") (\m' -> m'.formatDesc) m
     in Metadata
          { tags,
            mappedTags = either (const def) mapTags m,
            formatId = s.metadata_format,
            format = format
          }

mapTags :: F.Metadata -> MappedTags
mapTags F.Metadata {tags, lens} =
  MappedTags
    { artistName = mfilter (not . null) $ Just (V.toList $ tags ^. lens M.trackArtistTag),
      trackTitle = tags ^? lens M.trackTitle . _head,
      albumTitle = tags ^? lens M.album . _head,
      date = tags ^? lens M.year . _head,
      genre = mfilter (not . null) $ Just (V.toList $ tags ^. lens M.genre),
      albumArtist = mfilter (not . null) $ Just (V.toList $ tags ^. lens M.albumArtistTag),
      trackNumber = tags ^? lens M.trackNumber . _head,
      totalTracks = tags ^? lens M.totalTracksTag . _head <|> tags ^? lens M.trackTotalTag . _head,
      discNumber = tags ^? lens M.discNumberTag . _head,
      totalDiscs = tags ^? lens M.totalDiscsTag . _head <|> tags ^? lens M.discTotalTag . _head,
      comment = tags ^? lens M.commentTag . _head,
      musicbrainzArtistId = mfilter (not . null) $ Just (V.toList $ tags ^. lens MB.artistIdTag),
      musicbrainzAlbumArtistId = mfilter (not . null) $ Just (V.toList $ tags ^. lens MB.albumArtistIdTag),
      musicbrainzAlbumId = tags ^? lens MB.releaseIdTag . _head,
      musicbrainzTrackId = tags ^? lens MB.trackIdTag . _head
    }

data MappedTags = MappedTags
  { artistName :: Maybe [Text],
    trackTitle :: Maybe Text,
    albumTitle :: Maybe Text,
    date :: Maybe Text,
    genre :: Maybe [Text],
    albumArtist :: Maybe [Text],
    trackNumber :: Maybe Text,
    totalTracks :: Maybe Text,
    discNumber :: Maybe Text,
    totalDiscs :: Maybe Text,
    comment :: Maybe Text,
    musicbrainzArtistId :: Maybe [Text],
    musicbrainzAlbumArtistId :: Maybe [Text],
    musicbrainzAlbumId :: Maybe Text,
    musicbrainzTrackId :: Maybe Text
  }
  deriving (Generic)

instance GQLType MappedTags

instance Default MappedTags where
  def =
    MappedTags
      { artistName = Nothing,
        trackTitle = Nothing,
        albumTitle = Nothing,
        date = Nothing,
        genre = Nothing,
        albumArtist = Nothing,
        trackNumber = Nothing,
        totalTracks = Nothing,
        discNumber = Nothing,
        totalDiscs = Nothing,
        comment = Nothing,
        musicbrainzArtistId = Nothing,
        musicbrainzAlbumArtistId = Nothing,
        musicbrainzAlbumId = Nothing,
        musicbrainzTrackId = Nothing
      }

data MappedTagsInput = MappedTagsInput
  { __typename :: Maybe Text,
    artistName :: Maybe [Text],
    trackTitle :: Maybe Text,
    albumTitle :: Maybe Text,
    date :: Maybe Text,
    genre :: Maybe [Text],
    albumArtist :: Maybe [Text],
    trackNumber :: Maybe Text,
    totalTracks :: Maybe Text,
    discNumber :: Maybe Text,
    totalDiscs :: Maybe Text,
    comment :: Maybe Text,
    musicbrainzArtistId :: Maybe [Text],
    musicbrainzAlbumArtistId :: Maybe [Text],
    musicbrainzAlbumId :: Maybe Text,
    musicbrainzTrackId :: Maybe Text
  }
  deriving (Generic)

instance GQLType MappedTagsInput where
  type KIND MappedTagsInput = INPUT

-- unmapTags :: MappedTags INPUT -> F.Tags

resolveSourceGroups ::
  forall m o e.
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    WithOperation o
  ) =>
  Resolver o e m (V.Vector (SourceGroup (Resolver o e m)))
resolveSourceGroups = lift (getAll >>= (pure . fmap (enrichSourceEntity @m @o @e)) <&> groupSources)

resolveCollectionSourceGroups ::
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    WithOperation o
  ) =>
  Ty.CollectionRef ->
  Resolver o e m (Vector (SourceGroup (Resolver o e m)))
resolveCollectionSourceGroups collectionRef =
  lift (getCollectionSources collectionRef >>= (pure . fmap enrichSourceEntity) <&> groupSources)

data SourceGroup m = SourceGroup
  { groupTags :: GroupTags,
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
  { collectionId :: Ty.CollectionRef
  }
  deriving (Generic)

instance GQLType SourceGroupStreamArgs

streamSourceGroups :: CollectionWatchState -> Pool Connection -> Ty.CollectionRef -> L.ByteString -> StreamingBody
streamSourceGroups collectionWatchState pool collectionRef rq sendChunk flush =
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
    query = do
      srcs <- orderByUri $ Rel8.each sourceSchema
      Rel8.where_ (srcs.collection_id ==. Rel8.lit collectionRef)
      pure srcs
    streamSession :: Connection -> IO (Either QueryError ())
    streamSession =
      run $
        transactionIO ReadCommitted ReadOnly NotDeferrable $
          cursorTransactionIO $
            processStream (selectStream query)
    processStream :: MonadIO m => S.Stream (S.Of Ty.SourceEntity) m () -> m ()
    processStream s = do
      $(logInfoIO) $ "Starting streaming sources from collection " <> show collectionRef
      s
        & S.map (enrichSourceEntityIO collectionWatchState pool)
        & S.groupBy sameGroup
        & S.mapped S.toList
        & S.map toSourceGroup
        & S.mapM (\srcGrp -> liftIO $ interpreter (mkRoot srcGrp) (L.toStrict rq))
        & S.mapM_ sendFlush
      $(logInfoIO) $ "Finished streaming sources from collection " <> show collectionRef
    sendFlush :: MonadIO m => BS.ByteString -> m ()
    sendFlush = (liftIO . (const flush)) <=< liftIO . sendChunk . (`append` (putStringUtf8 "\n\n")) . fromByteString

toSourceGroup :: forall m o e. (MonadIO m, WithOperation o) => [Source (Resolver o e m)] -> SourceGroup (Resolver o e m)
toSourceGroup sources =
  SourceGroup
    { groupParentUri,
      coverImage,
      groupTags = groupMappedTags src.metadata.mappedTags,
      sources = sortBy (compareNaturalBy srcTrackNum) sources
    }
  where
    src = head sources
    groupParentUri = getParentUri src.sourceUri
    srcTrackNum :: Source n -> T.Text
    srcTrackNum src = fromMaybe src.sourceUri src.metadata.mappedTags.trackNumber
    coverImage :: Resolver o e m (Maybe Image)
    coverImage = case parseURI (T.unpack groupParentUri) >>= uriToFilePath of
      Just dir ->
        lift (runFileSystemIO $ findCoverImage dir) >>= \case
          Nothing -> src.coverImage
          Just imgPath ->
            pure $
              Just
                ExternalImage
                  { fileName = T.pack $ takeFileName imgPath,
                    downloadUri = "/source/" <> toText (coerce (src.id)) <> "/image"
                  }
      Nothing -> pure Nothing

enrichSourceEntityIO ::
  CollectionWatchState ->
  Pool Connection ->
  Ty.SourceEntity ->
  Source (Resolver QUERY () IO)
enrichSourceEntityIO collectionWatchState pool s =
  let filePath = parseURI (T.unpack s.source_uri) >>= uriToFilePath
   in Source
        { id = s.id,
          format = s.kind,
          metadata = from s,
          sourceName = fromMaybe s.source_uri $ T.pack . takeFileName <$> filePath,
          sourceUri = s.source_uri,
          filePath = T.pack <$> filePath,
          downloadUri = "/source/" <> toText (coerce s.id),
          length = lift $ sourceLengthImpl s filePath,
          coverImage = lift $ coverImageImpl s filePath,
          previewTransform = \args ->
            lift $
              previewTransformSourceIO collectionWatchState pool s args.transformations
        }
  where
    sourceLengthImpl :: Ty.SourceEntity -> Maybe FilePath -> IO (Maybe Double)
    sourceLengthImpl Ty.SourceTable {time_range = (Just intervalRange)} _ =
      pure $
        realToFrac . (* 1000) . nominalDiffTimeToSeconds <$> Ty.rangeLength intervalRange
    sourceLengthImpl _ (Just p) =
      runMetadataAggregateIO (openMetadataFile p) >>= \case
        Left e -> do
          $(logWarnIO) $ "failed to open file " <> p <> ": " <> E.displayException e
          pure Nothing
        Right mf -> pure $ F.audioLengthMilliseconds mf.audioInfo
    sourceLengthImpl _ _ = pure Nothing
    coverImageImpl :: Ty.SourceEntity -> Maybe FilePath -> IO (Maybe Image)
    coverImageImpl _ Nothing = pure Nothing
    coverImageImpl src (Just path) =
      let downloadUri = "/source/" <> toText (coerce src.id) <> "/image"
       in runFileSystemIO (findCoverImage (takeDirectory path)) >>= \case
            Just imgPath ->
              pure $
                Just
                  ExternalImage
                    { fileName = T.pack $ takeFileName imgPath,
                      downloadUri
                    }
            Nothing ->
              pure (EmbeddedImage <$> coerce src.cover <*> pure downloadUri)

previewTransformSourceIO ::
  CollectionWatchState ->
  Pool Connection ->
  Ty.SourceEntity ->
  Vector Transform ->
  IO (UpdateSourceResult (Resolver QUERY () IO))
previewTransformSourceIO collectionWatchState pool s ts = do
  $(logDebugIO) $ "Preview; Transforming source " <> show s.id <> " with " <> show ts
  E.catchAny
    ( case tryFrom s of
        Left e -> E.throwM (into @Tr.TransformationError e)
        Right s' -> do
          previewTransformationIO (Tr.evalTransformActions (interpretTransforms ts) s') >>= \case
            Left e -> E.throwM e
            Right s'' -> pure $ UpdatedSource (enrichSourceEntityIO collectionWatchState pool (from s''))
    )
    ( \e -> do
        $(logError) $ E.displayException e
        pure $ FailedSourceUpdate s.id (T.pack $ E.displayException e)
    )
  where
    interpretTransforms :: Vector Transform -> Vector Tr.TransformAction
    interpretTransforms ts = V.mapMaybe (rightToMaybe . tryFrom) ts
    previewTransformationIO m = runStdoutLogging do
      sess <- liftIO newAPISession
      run' sess m
    run' sess = runFileSystemIO
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

sameGroup :: Source m -> Source m -> Bool
sameGroup a b =
  groupMappedTags a.metadata.mappedTags == groupMappedTags b.metadata.mappedTags
    && getParentUri a.sourceUri == getParentUri b.sourceUri

groupSources :: forall m o e. (FileSystem m, WithOperation o) => V.Vector (Source (Resolver o e m)) -> V.Vector (SourceGroup (Resolver o e m))
groupSources = V.fromList . toList . fmap trSrcGrp . foldl' acc S.empty
  where
    acc gs' src =
      let groupedTags = groupMappedTags $ src.metadata.mappedTags
          newGroup =
            SourceGroup'
              { groupTags = groupedTags,
                groupParentUri = getParentUri src.sourceUri,
                sources = S.singleton src
              }
       in case gs' of
            (gs :> g) ->
              if getParentUri src.sourceUri == g.groupParentUri && g.groupTags == groupedTags
                then gs |> (g & #sources <>~ S.singleton src)
                else gs |> g |> newGroup
            _empty -> S.singleton newGroup
    trSrcGrp :: SourceGroup' (Resolver o e m) -> SourceGroup (Resolver o e m)
    trSrcGrp g =
      SourceGroup
        { groupTags = g.groupTags,
          groupParentUri = g.groupParentUri,
          sources = toList $ S.unstableSortBy (compareNaturalBy srcTrackNum) g.sources,
          coverImage = coverImageImpl g
        }
    srcTrackNum :: Source n -> Text
    srcTrackNum src = fromMaybe src.sourceUri src.metadata.mappedTags.trackNumber
    coverImageImpl :: SourceGroup' (Resolver o e m) -> Resolver o e m (Maybe Image)
    coverImageImpl g = case parseURI (T.unpack g.groupParentUri) >>= uriToFilePath of
      Just dir ->
        lift (findCoverImage dir) >>= \case
          Nothing -> case S.lookup 0 g.sources of
            Just src -> src.coverImage
            _ -> pure Nothing
          Just imgPath ->
            case S.lookup 0 g.sources of
              Nothing -> pure Nothing
              Just src ->
                pure $
                  Just
                    ExternalImage
                      { fileName = T.pack $ takeFileName imgPath,
                        downloadUri = "/source/" <> toText (Ty.unSourceRef (src.id)) <> "/image"
                      }
      Nothing -> pure Nothing

data SourceGroup' m = SourceGroup'
  { groupTags :: GroupTags,
    groupParentUri :: Text,
    sources :: S.Seq (Source m)
  }
  deriving (Generic)

getParentUri :: Text -> Text
getParentUri srcUri = case parseURI (T.unpack srcUri) of
  Just uri -> case uriScheme uri of
    "file:" -> T.pack $ show $ fileUri $ takeDirectory $ unEscapeString (uriPath uri)
    _ -> srcUri
  Nothing -> srcUri

data GroupTags = GroupTags
  { albumArtist :: Maybe [Text],
    albumTitle :: Maybe Text,
    date :: Maybe Text,
    genre :: Maybe [Text],
    totalTracks :: Maybe Text,
    discNumber :: Maybe Text,
    totalDiscs :: Maybe Text,
    musicbrainzArtistId :: Maybe [Text],
    musicbrainzAlbumArtistId :: Maybe [Text],
    musicbrainzAlbumId :: Maybe Text
  }
  deriving (Eq, Generic)

instance GQLType GroupTags

groupMappedTags :: MappedTags -> GroupTags
groupMappedTags m =
  GroupTags
    { albumArtist = m.albumArtist <|> m.artistName,
      albumTitle = m.albumTitle,
      date = m.date,
      genre = m.genre,
      totalTracks = m.totalTracks,
      discNumber = m.discNumber,
      totalDiscs = m.totalDiscs,
      musicbrainzArtistId = m.musicbrainzArtistId,
      musicbrainzAlbumArtistId = m.musicbrainzAlbumArtistId,
      musicbrainzAlbumId = m.musicbrainzAlbumId
    }

data SourceUpdate = SourceUpdate
  { id :: Ty.SourceRef,
    updateTags :: TagUpdate
  }
  deriving (Generic)

instance GQLType SourceUpdate where
  type KIND SourceUpdate = INPUT

data TagUpdate = TagUpdate
  { setMappedTags :: Maybe MappedTagsInput,
    setTags :: Maybe [UpdatePair]
  }
  deriving (Generic)

instance GQLType TagUpdate where
  type KIND TagUpdate = INPUT

data UpdatePair = UpdatePair
  { key :: Text,
    value :: Text
  }
  deriving (Generic)

instance GQLType UpdatePair where
  type KIND UpdatePair = INPUT

newtype UpdateSourcesArgs = UpdateSourcesArgs
  { updates :: Vector SourceUpdate
  }
  deriving (Generic)

instance GQLType UpdateSourcesArgs where
  type KIND UpdateSourcesArgs = INPUT

type UpdatedSources m = Vector (UpdateSourceResult m)

data UpdateSourceResult m
  = UpdatedSource (Source m)
  | FailedSourceUpdate {id :: Ty.SourceRef, msg :: Text}
  deriving (Generic)

instance Typeable m => GQLType (UpdateSourceResult m)

updateSourcesImpl ::
  forall m e.
  ( Tr.MonadSourceTransform m,
    MonadConc m
  ) =>
  UpdateSourcesArgs ->
  ResolverM e m (UpdatedSources (Resolver MUTATION e m))
updateSourcesImpl (UpdateSourcesArgs updates) = lift do
  updates' <- enrich updates
  results <- forM updates' updateSource
  pure results
  where
    updateSource :: SourceUpdate' -> m (UpdateSourceResult (Resolver MUTATION e m))
    updateSource SourceUpdate' {..} =
      case parseURI (T.unpack originalSource.source_uri) >>= uriToFilePath of
        Just path -> do
          let metadataFileId = F.MetadataFileId originalSource.kind
          readMetadataFile metadataFileId path >>= \case
            Left e -> do
              let msg = "error reading metadata file: " <> T.pack (show e)
              $(logError) msg
              pure $ FailedSourceUpdate {id, msg}
            Right f -> do
              let !f' = f & #metadata . each %~ resolveMetadata updateTags
              $(logDebug) $ "saving metadata: " <> show (f'.metadata)
              writeMetadataFile f' path >>= \case
                Left e -> do
                  let msg = "error writing metadata file: " <> T.pack (show e)
                  $(logError) msg
                  pure $ FailedSourceUpdate {id, msg}
                Right f'' ->
                  case chooseMetadata (H.elems (f''.metadata)) of
                    Nothing -> do
                      let msg = T.pack $ "unable to choose metadata format for source '" <> show id <> "'"
                      $(logError) msg
                      pure $ FailedSourceUpdate {id, msg}
                    Just metadata -> do
                      let updatedSource =
                            originalSource
                              { Ty.metadata = JSONBEncoded $ from metadata.tags,
                                Ty.metadata_format = coerce $ F.formatId metadata
                              }
                      us <- update @Ty.SourceEntity (V.singleton updatedSource)
                      let !ss = enrichSourceEntity <$> us
                      pure $ UpdatedSource (ss V.! 0)
        Nothing -> do
          let msg = T.pack $ "invalid source id '" <> show id <> "'"
          $(logError) msg
          pure $ FailedSourceUpdate {id, msg}
    enrich :: Vector SourceUpdate -> m (Vector SourceUpdate')
    enrich us = do
      let srcIds = fmap (.id) us
      srcs <- getByKey @Ty.SourceEntity srcIds
      let srcs' = H.fromList $ fmap (\src -> (src.id, src)) $ V.toList srcs
      pure $
        V.mapMaybe
          ( \SourceUpdate {..} ->
              SourceUpdate' id <$> rightToMaybe (tryFrom updateTags) <*> H.lookup id srcs'
          )
          us
    resolveMetadata :: TagUpdateOp -> F.Metadata -> F.Metadata
    resolveMetadata (SetMappedTags m) metadata = metadata {F.tags = setTagsFromMapped (F.lens metadata) (F.Tags V.empty) m}
      where
        setTagsFromMapped :: (F.TagMapping -> F.TagLens) -> F.Tags -> MappedTagsInput -> F.Tags
        setTagsFromMapped tag ts MappedTagsInput {..} =
          ts
            & tag M.trackArtistTag .~ maybe V.empty (V.fromList . filter (not . T.null)) artistName
            & tag M.trackTitle .~ maybe V.empty V.singleton (mfilter (not . T.null) trackTitle)
            & tag M.album .~ maybe V.empty V.singleton (mfilter (not . T.null) albumTitle)
            & tag M.year .~ maybe V.empty V.singleton (mfilter (not . T.null) date)
            & tag M.genre .~ maybe V.empty (V.fromList . filter (not . T.null)) genre
            & tag M.albumArtistTag .~ maybe V.empty (V.fromList . filter (not . T.null)) albumArtist
            & tag M.trackNumber .~ maybe V.empty V.singleton (mfilter (not . T.null) trackNumber)
            & tag M.totalTracksTag .~ maybe V.empty V.singleton (mfilter (not . T.null) totalTracks)
            & tag M.trackTotalTag .~ maybe V.empty V.singleton (mfilter (not . T.null) totalTracks)
            & tag M.discNumberTag .~ maybe V.empty V.singleton (mfilter (not . T.null) discNumber)
            & tag M.totalDiscsTag .~ maybe V.empty V.singleton (mfilter (not . T.null) totalDiscs)
            & tag M.discTotalTag .~ maybe V.empty V.singleton (mfilter (not . T.null) totalDiscs)
            & tag M.commentTag .~ maybe V.empty V.singleton (mfilter (not . T.null) comment)
            & tag MB.artistIdTag .~ maybe V.empty (V.fromList . filter (not . T.null)) musicbrainzArtistId
            & tag MB.albumArtistIdTag .~ maybe V.empty (V.fromList . filter (not . T.null)) musicbrainzAlbumArtistId
            & tag MB.albumIdTag .~ maybe V.empty V.singleton (mfilter (not . T.null) musicbrainzAlbumId)
            & tag MB.trackIdTag .~ maybe V.empty V.singleton (mfilter (not . T.null) musicbrainzTrackId)
    resolveMetadata (SetTags ps) metadata = metadata {F.tags = F.Tags $ V.fromList $ ps <&> \(UpdatePair k v) -> (k, v)}

data SourceUpdate' = SourceUpdate'
  { id :: Ty.SourceRef,
    updateTags :: TagUpdateOp,
    originalSource :: Ty.SourceEntity
  }
  deriving (Generic)

data TagUpdateOp
  = SetMappedTags MappedTagsInput
  | SetTags [UpdatePair]

instance TryFrom TagUpdate TagUpdateOp where
  tryFrom = maybeTryFrom impl
    where
      impl TagUpdate {..} =
        SetMappedTags <$> setMappedTags
          <|> SetTags <$> setTags

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
  = SetMapping {mapping :: Text, values :: [Text]}
  | RemoveMappings {mappings :: [Text]}
  | Retain {mappings :: [Text]}
  deriving (Show, Generic)

instance GQLType MetadataTransformation where
  type KIND MetadataTransformation = INPUT

previewTransformSourceImpl ::
  forall m e o.
  ( Tr.MonadSourceTransform m,
    --    MonadConc m,
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
  es <- resolveSourceEntities (SourceArgs where')
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
