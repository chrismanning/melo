{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Basement.From
import Control.Algebra
import Control.Applicative
import Control.Lens hiding (from, (|>))
import Control.Monad
import Data.Aeson as A
import Data.Coerce
import Data.Default
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
import Data.UUID (fromText, toText)
import qualified Data.Vector as V
import Database.Beam as B hiding (char, insert)
import Database.Beam.Postgres (PgJSONB (..))
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import qualified Melo.Database.Model as DB
import Melo.Format ()
import qualified Melo.Format as F
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata ()
import Melo.Library.Source.Repo
import qualified Melo.Library.Source.Types as Ty
import qualified Melo.Lookup.MusicBrainz as MB
import Network.URI
import System.FilePath

resolveSources ::
  (Has SourceRepository sig m) =>
  SourcesArgs ->
  Res e m [Source (Res e m)]
resolveSources (SourceArgs (Just SourceWhere{..})) =
  case id of
    Just idExpr -> case idExpr of
      WhereEqExpr (EqExpr x) -> case fromText x of
        Just uuid -> lift $ fmap from <$> getSources [DB.SourceKey uuid]
        Nothing -> fail $ "invalid source id " <> show x
      WhereInExpr (InExpr x) -> case allJust (fmap fromText x) of
        Just uuids -> lift $ fmap from <$> getSources (DB.SourceKey <$> uuids)
        Nothing -> fail $ "invalid source id in " <> show x
      _ -> fail "invalid where clause for Source.id"
    Nothing -> case sourceUri of
      Just sourceUriExpr -> case sourceUriExpr of
        WhereEqExpr (EqExpr x) -> case parseURI (T.unpack x) of
          Just uri -> lift $ fmap from <$> getSourcesByUri [uri]
          Nothing -> fail $ "invalid source uri " <> show x
        WhereInExpr (InExpr x) -> case allJust (fmap (parseURI . T.unpack) x) of
          Just uris -> lift $ fmap from <$> getSourcesByUri uris
          Nothing -> fail $ "invalid source id in " <> show x
        _ -> fail "invalid where clause for Source.sourceUri"
      Nothing -> lift $ fmap (fmap from) getAllSources
    where
      allJust :: [Maybe a] -> Maybe [a]
      allJust [] = Just []
      allJust (Just a:as) = fmap (a :) (allJust as)
      allJust (Nothing:_) = Nothing
resolveSources _ =
  lift $
    fmap (fmap from) getAllSources

data Source m = Source
  { id :: Text,
    format :: Text,
    metadata :: Metadata,
    sourceName :: Text,
    sourceUri :: Text,
    downloadUri :: Text,
    length :: m Float
  }
  deriving (Generic, GQLType)

instance Applicative m => From Ty.Source (Source m) where
  from s =
    Source
      { id = toText $ s ^. #ref . coerced,
        format = "",
        metadata = from $ s ^. #metadata,
        sourceName = getSourceName (T.pack $ show $ s ^. #source),
        sourceUri = T.pack $ show $ s ^. #source,
        downloadUri = "/source/" <> toText (s ^. #ref . coerced),
        length = pure 100
      }

instance Applicative m => From DB.Source (Source m) where
  from s =
    Source
      { id = toText (s ^. #id),
        format = s ^. #kind,
        metadata = from s,
        sourceName = getSourceName (s ^. #source_uri),
        sourceUri = s ^. #source_uri,
        downloadUri = "/source/" <> toText (s ^. #id),
        length = pure 100
      }

getSourceName :: Text -> Text
getSourceName srcUri = fromMaybe srcUri $ do
  uri <- parseURI (T.unpack srcUri)
  T.pack . takeFileName <$> uriToFilePath uri

uriToFilePath :: URI -> Maybe FilePath
uriToFilePath uri =
  case uriScheme uri of
    "file:" -> Just $ unEscapeString (uriPath uri)
    _ -> Nothing

data SourcesArgs = SourceArgs {
  where' :: Maybe SourceWhere
} deriving (Generic)

instance GQLType SourcesArgs where
  type KIND SourcesArgs = INPUT

data SourceWhere = SourceWhere
  { id :: Maybe Where,
--    format :: Maybe Where,
--    metadata :: Maybe Metadata,
--    sourceName :: Maybe Where,
    sourceUri :: Maybe Where
  }
  deriving (Generic)

instance GQLType SourceWhere where
  type KIND SourceWhere = INPUT

data Where = WhereEqExpr EqExpr |
  WhereNotEqExpr NotEqExpr |
  WhereContainsExpr ContainsExpr |
  WhereInExpr InExpr
    deriving (Generic)

instance GQLType Where where
  type KIND Where = INPUT

newtype EqExpr = EqExpr {
  eq :: Text
} deriving (Generic)

instance GQLType EqExpr where
  type KIND EqExpr = INPUT

newtype NotEqExpr = NotEqExpr {
  notEq :: Text
} deriving (Generic)

instance GQLType NotEqExpr where
  type KIND NotEqExpr = INPUT

newtype ContainsExpr = ContainsExpr {
  contains :: Text
} deriving (Generic)

instance GQLType ContainsExpr where
  type KIND ContainsExpr = INPUT

newtype InExpr = InExpr {
  in' :: [Text]
} deriving (Generic)

instance GQLType InExpr where
  type KIND InExpr = INPUT

data TimeUnit = Seconds | Milliseconds | Nanoseconds
  deriving (Generic)

getSourceLength :: Monad m => URI -> TimeUnit -> m Float
getSourceLength uri unit = do
  undefined

data Metadata = Metadata
  { tags :: [(Text, Text)],
    mappedTags :: MappedTags,
    formatId :: Text,
    format :: Text
  }
  deriving (Generic, GQLType)

instance From F.Metadata Metadata where
  from m =
    Metadata
      { formatId = coerce $ F.formatId m,
        format = m ^. #formatDesc,
        tags = V.toList $ coerce $ F.tags m,
        mappedTags = mapTags m
      }

instance From DB.Source Metadata where
  from s =
    let s' :: Maybe Ty.Source = tryFrom s
        m :: Maybe F.Metadata = fmap (^. #metadata) s'
        PgJSONB (DB.SourceMetadata tags) = s ^. #metadata
        format = maybe "" (^. #formatDesc) m
     in Metadata
          { tags = V.toList tags,
            mappedTags = maybe def mapTags m,
            formatId = s ^. #metadata_format,
            format = format
          }

mapTags :: F.Metadata -> MappedTags
mapTags metadata =
  MappedTags
    { artistName = mfilter (not . null) $ Just (V.toList $ ts ^. tag M.trackArtistTag),
      trackTitle = ts ^. tag M.trackTitle ^? _head,
      albumTitle = ts ^. tag M.album ^? _head,
      date = ts ^. tag M.year ^? _head,
      genre = mfilter (not . null) $ Just (V.toList $ ts ^. tag M.genre),
      albumArtist = mfilter (not . null) $ Just (V.toList $ ts ^. tag M.albumArtistTag),
      trackNumber = ts ^. tag M.trackNumber ^? _head,
      totalTracks = ts ^. tag M.totalTracksTag ^? _head <|> ts ^. tag M.trackTotalTag ^? _head,
      discNumber = ts ^. tag M.discNumberTag ^? _head,
      totalDiscs = ts ^. tag M.totalDiscsTag ^? _head <|> ts ^. tag M.discTotalTag ^? _head,
      comment = ts ^. tag M.commentTag ^? _head,
      musicbrainzArtistId = mfilter (not . null) $ Just (V.toList $ ts ^. tag MB.artistIdTag),
      musicbrainzAlbumArtistId = mfilter (not . null) $ Just (V.toList $ ts ^. tag MB.albumArtistIdTag),
      musicbrainzAlbumId = ts ^. tag MB.releaseIdTag ^? _head,
      musicbrainzTrackId = ts ^. tag MB.trackIdTag ^? _head
    }
  where
    ts :: F.Tags
    ts = metadata ^. #tags
    tag = F.lens metadata

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
  deriving (Generic, ToJSON, GQLType)

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
  deriving (Generic, ToJSON)

instance GQLType MappedTagsInput where
  type KIND MappedTagsInput = INPUT

--unmapTags :: MappedTags INPUT -> F.Tags

resolveSourceGroups ::
  ( Has SourceRepository sig m,
    Has FileSystem sig m
  ) =>
  Res e m [SourceGroup (Res e m)]
resolveSourceGroups = lift $ groupSources <$!> fmap (fmap from) getAllSources

data SourceGroup m = SourceGroup
  { groupTags :: GroupTags,
    groupParentUri :: Text,
    sources :: [Source m],
    coverImage :: m (Maybe Image)
  }
  deriving (Generic, GQLType)

data Image = Image
  { fileName :: Text,
    downloadUri :: Text
  }
  deriving (Generic, GQLType)

data SourceContent
  = Folder [SourceContent]
  | ImageContent
      { fileName :: Text,
        downloadUri :: Text
      }
  deriving (Generic, GQLType)

groupSources :: forall m sig e. (Has FileSystem sig m) => [Source (Res e m)] -> [SourceGroup (Res e m)]
groupSources = fmap trSrcGrp . toList . foldl' acc S.empty
  where
    acc gs' src =
      let groupedTags = groupMappedTags $ src ^. #metadata . #mappedTags
          newGroup =
            SourceGroup'
              { groupTags = groupedTags,
                groupParentUri = getParentUri (src ^. #sourceUri),
                sources = S.singleton src
              }
       in case gs' of
            (gs :> g) ->
              if getParentUri (src ^. #sourceUri) == g ^. #groupParentUri && g ^. #groupTags == groupedTags
                then gs |> (g & #sources <>~ S.singleton src)
                else gs |> g |> newGroup
            _ -> S.singleton newGroup
    trSrcGrp :: SourceGroup' (Res e m) -> SourceGroup (Res e m)
    trSrcGrp g =
      SourceGroup
        { groupTags = g ^. #groupTags,
          groupParentUri = g ^. #groupParentUri,
          sources = toList $ g ^. #sources,
          coverImage = lift $ pure Nothing -- coverImageImpl g
        }
    coverImageImpl :: SourceGroup' (Res e m) -> m (Maybe Image)
    coverImageImpl g = case parseURI $ T.unpack $ g ^. #groupParentUri of
      Just uri ->
        case uriToFilePath uri of
          Just dir -> do
            imgPath <- findCoverImage dir
            undefined
          Nothing -> undefined
      Nothing -> pure Nothing

findCoverImage :: Has FileSystem sig m => FilePath -> m (Maybe FilePath)
findCoverImage p = do
  isDir <- doesDirectoryExist p
  if isDir
    then do
      entries <- listDirectory p
      undefined
    else pure Nothing

data SourceGroup' m = SourceGroup'
  { groupTags :: GroupTags,
    groupParentUri :: Text,
    sources :: S.Seq (Source m)
  }
  deriving (Generic)

getParentUri :: Text -> Text
getParentUri srcUri = case parseURI (T.unpack srcUri) of
  Just uri -> case uriScheme uri of
    "file:" -> T.pack $ show $ Ty.fileUri $ takeDirectory $ unEscapeString (uriPath uri)
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
  deriving (Eq, Generic, GQLType)

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
  { id :: Text,
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
  { updates :: [SourceUpdate]
  }
  deriving (Generic)

instance GQLType UpdateSourcesArgs where
  type KIND UpdateSourcesArgs = INPUT

newtype UpdatedSources = UpdatedSources
  { results :: [UpdateSourceResult]
  }
  deriving (Generic)
  deriving anyclass (GQLType)

instance Semigroup UpdatedSources where
  a <> b =
    UpdatedSources
      { results = (a ^. #results) <> (b ^. #results)
      }

instance Monoid UpdatedSources where
  mempty =
    UpdatedSources
      { results = []
      }

data UpdateSourceResult
  = UpdatedSource {id :: Text}
  | FailedSourceUpdate {id :: Text, msg :: Text}
  deriving (Generic, GQLType)

updateSourcesImpl ::
  forall sig m e.
  ( Has SourceRepository sig m,
    Has Logging sig m,
    Has MetadataService sig m
  ) =>
  UpdateSourcesArgs ->
  MutRes e m UpdatedSources
updateSourcesImpl (UpdateSourcesArgs updates) = do
  updates' <- lift $ enrich updates
  results <- lift $ forM updates' updateSource
  pure UpdatedSources {results}
  where
    updateSource :: SourceUpdate' -> m UpdateSourceResult
    updateSource SourceUpdate' {..} =
      case fromText id of
        Nothing -> do
          let msg = "invalid source id '" <> id <> "'; expected UUID"
          $(logError) msg
          pure $ FailedSourceUpdate {id, msg}
        Just key ->
          case parseURI (T.unpack $ originalSource ^. #source_uri) >>= uriToFilePath of
            Just path -> do
              let metadataFileId = F.MetadataFileId $ originalSource ^. #kind
              -- TODO catch exceptions with fused-effects-exceptions
              f <-
                readMetadataFile metadataFileId path
                  <&> #metadata . each %~ resolveMetadata updateTags
              f <- writeMetadataFile f path
              case chooseMetadata (H.elems (f ^. #metadata)) of
                Nothing -> do
                  let msg = T.pack $ "unable to choose metadata format for source '" <> show id <> "'"
                  $(logError) msg
                  pure $ FailedSourceUpdate {id, msg}
                Just metadata -> do
                  let F.Tags tags = F.tags metadata
                  let updatedSource =
                        originalSource
                          { DB.metadata = PgJSONB $ DB.SourceMetadata tags,
                            DB.metadata_format = coerce $ F.formatId metadata
                          }
                  updateSources [updatedSource]
                  pure
                    UpdatedSource
                      { id = toText key
                      }
            Nothing -> do
              let msg = T.pack $ "invalid source id '" <> show id <> "'"
              $(logError) msg
              pure $ FailedSourceUpdate {id, msg}
    enrich :: [SourceUpdate] -> m [SourceUpdate']
    enrich us = do
      let srcIds :: [DB.SourceKey] = DB.SourceKey <$> catMaybes (fromText <$> fmap (^. #id) us)
      srcs <- getSources srcIds
      let srcs' = H.fromList $ fmap (\src -> (toText $ src ^. #id, src)) srcs
      pure $
        mapMaybe
          ( \SourceUpdate {..} ->
              SourceUpdate' id <$> tryFrom updateTags <*> H.lookup id srcs'
          )
          us
    resolveMetadata :: TagUpdateOp -> F.Metadata -> F.Metadata
    resolveMetadata (SetMappedTags m) metadata = metadata {F.tags = setTagsFromMapped (F.lens metadata) (F.Tags V.empty) m}
      where
        setTagsFromMapped :: (F.TagMapping -> F.TagLens) -> F.Tags -> MappedTagsInput -> F.Tags
        setTagsFromMapped tag ts MappedTagsInput {..} = ts
          & tag M.artist .~ maybe V.empty V.fromList artistName
          & tag M.trackTitle .~ maybe V.empty V.singleton trackTitle
          & tag M.album .~ maybe V.empty V.singleton albumTitle
          & tag M.year .~ maybe V.empty V.singleton date
          & tag M.genre .~ maybe V.empty V.fromList genre
          & tag M.albumArtist .~ maybe V.empty V.fromList albumArtist
          & tag M.trackNumber .~ maybe V.empty V.singleton trackNumber
          & tag M.totalTracksTag .~ maybe V.empty V.singleton totalTracks
          & tag M.trackTotalTag .~ maybe V.empty V.singleton totalTracks
          & tag M.discNumberTag .~ maybe V.empty V.singleton discNumber
          & tag M.totalDiscsTag .~ maybe V.empty V.singleton totalDiscs
          & tag M.discTotalTag .~ maybe V.empty V.singleton totalDiscs
          & tag M.commentTag .~ maybe V.empty V.singleton comment
          & tag MB.artistIdTag .~ maybe V.empty V.fromList musicbrainzArtistId
          & tag MB.albumArtistIdTag .~ maybe V.empty V.fromList musicbrainzAlbumArtistId
          & tag MB.albumIdTag .~ maybe V.empty V.singleton musicbrainzAlbumId
          & tag MB.trackIdTag .~ maybe V.empty V.singleton musicbrainzTrackId
          & tag M.commentTag .~ maybe V.empty V.singleton comment
    resolveMetadata (SetTags ps) metadata = metadata {F.tags = F.Tags $ V.fromList $ ps <&> \(UpdatePair k v) -> (k, v)}

data SourceUpdate' = SourceUpdate'
  { id :: Text,
    updateTags :: TagUpdateOp,
    originalSource :: DB.Source
  }
  deriving (Generic)

data TagUpdateOp
  = SetMappedTags MappedTagsInput
  | SetTags [UpdatePair]

instance TryFrom TagUpdate TagUpdateOp where
  tryFrom TagUpdate {..} =
    SetMappedTags <$> setMappedTags
      <|> SetTags <$> setTags
