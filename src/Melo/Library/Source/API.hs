{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Control.Applicative hiding (many)
import Control.Exception.Safe (toException)
import Control.Lens hiding (from, lens, (|>))
import Control.Monad
import Data.Aeson as A hiding (Result, (<?>))
import Data.Coerce
import Data.Default
import Data.Either.Combinators (mapLeft, mapRight, rightToMaybe)
import Data.Foldable
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Sequence ((|>))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.UUID (fromText, toText)
import qualified Data.Vector as V
import Data.Vector (Vector)
import GHC.Generics hiding (from)
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Format ()
import qualified Melo.Format as F
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata ()
import Melo.GraphQL.Where
import qualified Melo.Library.Collection.Types as Ty
import Melo.Library.Source.Repo
import qualified Melo.Library.Source.Transform as Tr
import qualified Melo.Library.Source.Types as Ty
import qualified Melo.Lookup.MusicBrainz as MB
import Rel8 (JSONBEncoded(..))
import System.FilePath as P
import Witch

resolveSources ::
  (SourceRepository m, FileSystem m, WithOperation o) =>
  SourcesArgs ->
  Resolver o e m (Vector (Source (Resolver o e m)))
resolveSources args = fmap (fmap from) (resolveSourcesImpl args)

resolveSourcesImpl ::
  (SourceRepository m, FileSystem m, WithOperation o) =>
  SourcesArgs ->
  Resolver o e m (Vector Ty.Source)
resolveSourcesImpl (SourceArgs (Just SourceWhere {..})) =
  case id of
    Just idExpr -> case idExpr of
      WhereEqExpr (EqExpr x) -> case fromText x of
        Just uuid -> lift $ convertSources <$> getByKey (V.singleton (Ty.SourceRef uuid))
        Nothing -> fail $ "invalid source id " <> show x
      WhereInExpr (InExpr x) -> case allJust (fmap fromText x) of
        Just uuids -> lift $ convertSources <$> getByKey (V.fromList $ Ty.SourceRef <$> uuids)
        Nothing -> fail $ "invalid source id in " <> show x
      _unknownWhere -> fail "invalid where clause for Source.id"
    Nothing -> case sourceUri of
      Just sourceUriExpr -> case sourceUriExpr of
        WhereEqExpr (EqExpr x) -> case parseURI (T.unpack x) of
          Just uri -> lift $ convertSources <$> getByUri (V.singleton uri)
          Nothing -> fail $ "invalid source uri " <> show x
        WhereInExpr (InExpr x) -> case allJust (fmap (parseURI . T.unpack) x) of
          Just uris -> lift $ convertSources <$> getByUri (V.fromList uris)
          Nothing -> fail $ "invalid source uri in " <> show x
        WhereStartsWithExpr (StartsWithExpr x) -> case parseURI $ T.unpack x of
          Just uri -> lift $ convertSources <$> getByUriPrefix uri
          Nothing -> fail $ "invalid source uri in " <> show x
        _unknownWhere -> fail "invalid where clause for Source.sourceUri"
      Nothing -> lift $ fmap convertSources getAll
  where
    allJust :: [Maybe a] -> Maybe [a]
    allJust [] = Just []
    allJust (Just a : as) = fmap (a :) (allJust as)
    allJust (Nothing : _) = Nothing
resolveSourcesImpl _ = lift $ fmap convertSources getAll

convertSources :: Vector Ty.SourceEntity -> Vector Ty.Source
convertSources = V.mapMaybe (rightToMaybe . tryFrom)

resolveCollectionSources ::
  (SourceRepository m, FileSystem m, WithOperation o) =>
  Ty.CollectionRef ->
  CollectionSourcesArgs ->
  Resolver o e m (Vector (Source (Resolver o e m)))
resolveCollectionSources collectionRef _args =
  -- TODO handle args
  lift $ fmap (fmap from) $ getCollectionSources collectionRef

data Source m = Source
  { id :: Ty.SourceRef,
    format :: Text,
    metadata :: Metadata,
    sourceName :: Text,
    sourceUri :: Text,
    downloadUri :: Text,
    length :: m Double,
    coverImage :: m (Maybe Image)
  }
  deriving (Generic)

instance Typeable m => GQLType (Source m)

instance (Applicative m, FileSystem m, WithOperation o) => From Ty.Source (Source (Resolver o e m)) where
  from = via @Ty.SourceEntity

instance (Applicative m, FileSystem m, WithOperation o) =>
  From Ty.SourceEntity (Source (Resolver o e m)) where
  from s =
    Source
      { id = s ^. #id,
        format = s ^. #kind,
        metadata = from s,
        sourceName = getSourceName (s ^. #source_uri),
        sourceUri = s ^. #source_uri,
        downloadUri = "/source/" <> toText (Ty.unSourceRef (s ^. #id)),
        length = pure 100,
        coverImage = lift $ coverImageImpl s
      }
    where
      coverImageImpl :: Ty.SourceEntity -> m (Maybe Image)
      coverImageImpl src = case parseURI $ T.unpack $ src ^. #source_uri of
        Just uri ->
          case uriToFilePath uri of
            Just path ->
              findCoverImage (takeDirectory path) >>= \case
                Nothing -> pure Nothing
                Just imgPath ->
                  pure $
                    Just
                      Image
                        { fileName = T.pack $ takeFileName imgPath,
                          downloadUri = "/source/" <> toText (Ty.unSourceRef (src ^. #id)) <> "/image"
                        }
            Nothing -> pure Nothing
        Nothing -> pure Nothing

getSourceName :: Text -> Text
getSourceName srcUri = fromMaybe srcUri $ do
  uri <- parseURI (T.unpack srcUri)
  T.pack . takeFileName <$> uriToFilePath uri

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
  { tags :: V.Vector Tag,
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
        tags = from @(Text, Text) <$> coerce m.tags,
        mappedTags = mapTags m
      }

instance From Ty.SourceEntity Metadata where
  from s =
    let s' = tryFrom @_ @Ty.Source s
        m = fmap (\s'' -> s''.metadata) s'
        JSONBEncoded (Ty.SourceMetadata tags) = s.metadata
        format = either (const "") (\m' -> m'.formatDesc) m
     in Metadata
          { tags = from <$> tags,
            mappedTags = either (const def) mapTags m,
            formatId = s.metadata_format,
            format = format
          }

data Tag = Tag {
  key :: Text,
  value :: Text
} deriving (Generic)

instance GQLType Tag

instance From (Text, Text) Tag where
  from (k, v) = Tag k v

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

instance ToJSON MappedTags

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

instance ToJSON MappedTagsInput

instance GQLType MappedTagsInput where
  type KIND MappedTagsInput = INPUT

--unmapTags :: MappedTags INPUT -> F.Tags

resolveSourceGroups ::
  ( SourceRepository m,
    FileSystem m,
    WithOperation o
  ) =>
  Resolver o e m (V.Vector (SourceGroup (Resolver o e m)))
resolveSourceGroups = lift $ groupSources <$!> fmap (fmap from) getAll

resolveCollectionSourceGroups ::
  ( SourceRepository m,
    FileSystem m,
    WithOperation o
  ) =>
  Ty.CollectionRef ->
  Resolver o e m (Vector (SourceGroup (Resolver o e m)))
resolveCollectionSourceGroups collectionRef =
  lift $ groupSources <$!> fmap (fmap from) (getCollectionSources collectionRef)

data SourceGroup m = SourceGroup
  { groupTags :: GroupTags,
    groupParentUri :: Text,
    sources :: [Source m],
    coverImage :: m (Maybe Image)
  }
  deriving (Generic)

instance Typeable m => GQLType (SourceGroup m)

data Image = Image
  { fileName :: Text,
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

groupSources :: forall m e o. (FileSystem m, WithOperation o) => V.Vector (Source (Resolver o e m)) -> V.Vector (SourceGroup (Resolver o e m))
groupSources = V.fromList . toList . fmap trSrcGrp . foldl' acc S.empty
  where
    acc gs' src =
      let groupedTags = groupMappedTags $ src.metadata.mappedTags
          newGroup =
            SourceGroup'
              { groupTags = groupedTags,
                groupParentUri = getParentUri (src ^. #sourceUri),
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
          sources = toList $ g.sources,
          coverImage = lift $ coverImageImpl g
        }
    coverImageImpl :: SourceGroup' (Resolver o e m) -> m (Maybe Image)
    coverImageImpl g = case parseURI $ T.unpack $ g.groupParentUri of
      Just uri ->
        case uriToFilePath uri of
          Just dir ->
            findCoverImage dir >>= \case
              Nothing -> pure Nothing
              Just imgPath ->
                case S.lookup 0 $ g.sources of
                  Nothing -> pure Nothing
                  Just src ->
                    pure $
                      Just
                        Image
                          { fileName = T.pack $ takeFileName imgPath,
                            downloadUri = "/source/" <> toText (Ty.unSourceRef (src.id)) <> "/image"
                          }
          Nothing -> pure Nothing
      Nothing -> pure Nothing

findCoverImage :: FileSystem m => FilePath -> m (Maybe FilePath)
findCoverImage p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- listDirectory p
      pure $
        find (\e -> P.takeBaseName e == "cover" && isImage e) entries
          <|> find (\e -> P.takeBaseName e == "front" && isImage e) entries
          <|> find (\e -> P.takeBaseName e == "folder" && isImage e) entries
    else pure Nothing
  where
    isImage :: FilePath -> Bool
    isImage p =
      let ext = takeExtension p
       in ext == ".jpeg"
            || ext == ".jpg"
            || ext == ".png"

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
    { albumArtist = m ^. #albumArtist <|> m ^. #artistName,
      albumTitle = m ^. #albumTitle,
      date = m ^. #date,
      genre = m ^. #genre,
      totalTracks = m ^. #totalTracks,
      discNumber = m ^. #discNumber,
      totalDiscs = m ^. #totalDiscs,
      musicbrainzArtistId = m ^. #musicbrainzArtistId,
      musicbrainzAlbumArtistId = m ^. #musicbrainzAlbumArtistId,
      musicbrainzAlbumId = m ^. #musicbrainzAlbumId
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

newtype UpdatedSources m = UpdatedSources
  { results :: Vector (UpdateSourceResult m)
  }
  deriving (Generic)

instance Typeable m => GQLType (UpdatedSources m)

instance Semigroup (UpdatedSources m) where
  a <> b =
    UpdatedSources
      { results = (a ^. #results) <> (b ^. #results)
      }

instance Monoid (UpdatedSources m) where
  mempty =
    UpdatedSources
      { results = V.empty
      }

data UpdateSourceResult m
  = UpdatedSource (Source m)
  | FailedSourceUpdate {id :: Ty.SourceRef, msg :: Text}
  deriving (Generic)

instance Typeable m => GQLType (UpdateSourceResult m)

updateSourcesImpl ::
  forall m e.
  ( SourceRepository m,
    FileSystem m,
    Logging m,
    MetadataService m
  ) =>
  UpdateSourcesArgs ->
  ResolverM e m (UpdatedSources (Resolver MUTATION e m))
updateSourcesImpl (UpdateSourcesArgs updates) = lift do
  updates' <- enrich updates
  results <- forM updates' updateSource
  pure UpdatedSources {results}
  where
    updateSource :: SourceUpdate' -> m (UpdateSourceResult (Resolver MUTATION e m))
    updateSource SourceUpdate' {..} =
          case parseURI (T.unpack $ originalSource ^. #source_uri) >>= uriToFilePath of
            Just path -> do
              let metadataFileId = F.MetadataFileId $ originalSource ^. #kind
              readMetadataFile metadataFileId path >>= \case
                Left e -> do
                  let msg = "error reading metadata file: " <> T.pack (show e)
                  $(logError) msg
                  pure $ FailedSourceUpdate {id, msg}
                Right f -> do
                  let !f' = f & #metadata . each %~ resolveMetadata updateTags
                  $(logDebug) $ "saving metadata: " <> show (f' ^. #metadata)
                  writeMetadataFile f' path >>= \case
                    Left e -> do
                      let msg = "error writing metadata file: " <> T.pack (show e)
                      $(logError) msg
                      pure $ FailedSourceUpdate {id, msg}
                    Right f'' ->
                      case chooseMetadata (H.elems (f'' ^. #metadata)) of
                        Nothing -> do
                          let msg = T.pack $ "unable to choose metadata format for source '" <> show id <> "'"
                          $(logError) msg
                          pure $ FailedSourceUpdate {id, msg}
                        Just metadata -> do
                          let F.Tags tags = F.tags metadata
                          let updatedSource =
                                originalSource
                                  { Ty.metadata = JSONBEncoded $ Ty.SourceMetadata tags,
                                    Ty.metadata_format = coerce $ F.formatId metadata
                                  }
                          update (V.singleton updatedSource)
                          pure $ UpdatedSource (from updatedSource)
            Nothing -> do
              let msg = T.pack $ "invalid source id '" <> show id <> "'"
              $(logError) msg
              pure $ FailedSourceUpdate {id, msg}
    enrich :: Vector SourceUpdate -> m (Vector SourceUpdate')
    enrich us = do
      let srcIds = fmap (^. #id) us
      srcs <- getByKey @Ty.SourceEntity srcIds
      let srcs' = H.fromList $ fmap (\src -> (src ^. #id, src)) $ V.toList srcs
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

type TransformSources m = TransformSourcesArgs -> m (UpdatedSources m)

data TransformSourcesArgs = TransformSourcesArgs
  { transformations :: Vector Transform,
    where' :: Maybe SourceWhere
  }
  deriving (Generic)

instance GQLType TransformSourcesArgs where
  type KIND TransformSourcesArgs = INPUT

data Transform =
    Move { movePattern :: Text }
  | SplitMultiTrackFile { movePattern :: Text}
  | EditMetadata { metadataTransform :: MetadataTransformation }
  deriving (Show, Generic)

instance GQLType Transform where
  type KIND Transform = INPUT

data MetadataTransformation =
    SetMapping { mapping :: Text, values :: [Text] }
  | RemoveMappings { mappings :: [Text] }
  | Retain { mappings :: [Text] }
--  | TitleCase { mappings :: [Text] }
  deriving (Show, Generic)

instance GQLType MetadataTransformation where
  type KIND MetadataTransformation = INPUT

previewTransformSourcesImpl ::
  forall m e.
    ( SourceRepository m,
      Logging m,
      MetadataService m,
      Tr.MonadSourceTransform m
    ) =>
    TransformSourcesArgs ->
    ResolverM e m (UpdatedSources (Resolver MUTATION e m))
previewTransformSourcesImpl (TransformSourcesArgs ts where') = do
  ss <- resolveSourcesImpl (SourceArgs where')
  $(logDebug) $ "Preview; Transforming sources with " <> show ts
  ss' <- Tr.previewTransformActions (interpretTransforms ts) (from <$> ss)
  pure $ UpdatedSources (UpdatedSource . from <$> ss')
  where
    interpretTransforms :: Vector Transform -> Vector Tr.TransformAction
    interpretTransforms ts = V.mapMaybe (rightToMaybe . tryFrom) ts

transformSourcesImpl ::
  forall m e.
    ( SourceRepository m,
      Logging m,
      MetadataService m,
      Tr.MonadSourceTransform m
    ) =>
    TransformSourcesArgs ->
    ResolverM e m (UpdatedSources (Resolver MUTATION e m))
transformSourcesImpl (TransformSourcesArgs ts where') = do
  $(logDebug) $ "Transforming sources with " <> show ts
  ss <- resolveSourcesImpl (SourceArgs where')
  ss' <- Tr.evalTransformActions (interpretTransforms ts) (from <$> ss)
  pure $ UpdatedSources (UpdatedSource . from <$> ss')
  where
    interpretTransforms :: Vector Transform -> Vector Tr.TransformAction
    interpretTransforms ts = V.mapMaybe (rightToMaybe . tryFrom) ts

instance TryFrom Transform Tr.TransformAction where
  tryFrom t = case t of
    Move pat -> Tr.Move <$> parseMovePattern' pat
    SplitMultiTrackFile pat -> Tr.SplitMultiTrackFile <$> parseMovePattern' pat
    EditMetadata mt -> Right $ Tr.EditMetadata (from mt)
    where
      parseMovePattern' pat = mapLeft (TryFromException t <$> fmap toException) $ Tr.parseMovePattern pat

instance From MetadataTransformation Tr.MetadataTransformation where
  from t@(SetMapping m vs) = Tr.SetMapping m vs
  from (RemoveMappings ms) = Tr.RemoveMappings ms
  from (Retain ms) = Tr.Retain ms
