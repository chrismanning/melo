{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

import Basement.From
import Control.Algebra
import Control.Applicative
import Control.Carrier.Lift
import Control.Effect.Reader
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
import Database.Beam.Postgres as Pg
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import qualified Melo.Database.Model as DB
import Melo.Database.Query
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
  Res e m [Source (Res e m)]
resolveSources =
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

data TimeUnit = Seconds | Milliseconds | Nanoseconds
  deriving (Generic)

getSourceLength :: Monad m => URI -> TimeUnit -> m Float
getSourceLength uri unit = do
  undefined

data Metadata = Metadata
  { tags :: [(Text, Text)],
    mappedTags :: MappedTags OUTPUT,
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
        tags = fromMaybe [] $ toMaybe $ fromJSON (s ^. #metadata . fromPg)
        format = maybe "" (^. #formatDesc) m
     in Metadata
          { tags,
            mappedTags = maybe def mapTags m,
            formatId = s ^. #metadata_format,
            format = format
          }

mapTags :: F.Metadata -> MappedTags t
mapTags metadata =
  MappedTags
    { artistName = mfilter (not . null) $ Just (V.toList $ ts ^. tag M.artist),
      trackTitle = ts ^. tag M.trackTitle ^? _head,
      albumTitle = ts ^. tag M.album ^? _head,
      date = ts ^. tag M.year ^? _head,
      genre = mfilter (not . null) $ Just (V.toList $ ts ^. tag M.genre),
      albumArtist = mfilter (not . null) $ Just (V.toList $ ts ^. tag M.albumArtist),
      trackNumber = ts ^. tag M.trackNumber ^? _head,
      totalTracks = Nothing, -- ts ^. tag M.totalTracks ^? _head,
      discNumber = Nothing, -- ts ^. tag M.discNumber ^? _head,
      totalDiscs = Nothing, -- ts ^. tag M.totalDiscs ^? _head,
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

data MappedTags (t :: GQL_KIND) = MappedTags
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
  deriving (Generic, ToJSON)

instance Default (MappedTags t) where
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

instance GQLType (MappedTags OUTPUT)

instance GQLType (MappedTags INPUT) where
  type KIND (MappedTags INPUT) = INPUT

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

--instance From SourceGroup' SourceGroup where
--  from g =
--    SourceGroup
--      { groupTags = g ^. #groupTags,
--        groupParentUri = g ^. #groupParentUri,
--        sources = toList $ g ^. #sources
--      }

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

groupMappedTags :: MappedTags a -> GroupTags
groupMappedTags m =
  GroupTags
    { albumArtist = m ^. #albumArtist,
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

toMaybe :: Result a -> Maybe a
toMaybe = \case
  Success a -> Just a
  Error _ -> Nothing

fromPg = to fromPg'

fromPg' :: PgJSONB a -> a
fromPg' (PgJSONB a) = a

data SourceUpdate = SourceUpdate
  { id :: Text,
    updateTags :: TagUpdate
  }
  deriving (Generic)

instance GQLType SourceUpdate where
  type KIND SourceUpdate = INPUT

data TagUpdate = TagUpdate
  { setMappedTags :: Maybe (MappedTags INPUT),
    setTags :: Maybe [UpdatePair],
    updateMappedTags :: Maybe (MappedTags INPUT),
    updateTags :: Maybe [UpdatePair]
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
    Has MetadataService sig m,
    Has (Reader Connection) sig m,
    Has (Lift IO) sig m
  ) =>
  UpdateSourcesArgs ->
  MutRes e m UpdatedSources
updateSourcesImpl (UpdateSourcesArgs updates) = do
  updates' <- lift $ enrich updates
  results <- lift $ forM updates' updateSource
  pure UpdatedSources {results}
  where
    updateSource :: SourceUpdate' -> m UpdateSourceResult
    updateSource update =
      case fromText $ update ^. #id of
        Nothing -> do
          let id = update ^. #id
          let msg = "invalid source id '" <> id <> "'; expected UUID"
          $(logError) msg
          pure $ FailedSourceUpdate {id, msg}
        Just key ->
          case parseURI (T.unpack $ update ^. #originalSource . #source_uri) >>= uriToFilePath of
            Just path -> do
              let metadataFileId = F.MetadataFileId $ update ^. #originalSource . #kind
              -- TODO catch exceptions with fused-effects-exceptions
              f <-
                readMetadataFile metadataFileId path
                  <&> #metadata . each %~ resolveMetadata (update ^. #updateTags)
              f <- writeMetadataFile f path
              case chooseMetadata (H.elems (f ^. #metadata)) of
                Nothing -> do
                  let id = update ^. #id
                  let msg = T.pack $ "unable to choose metadata format for source '" <> show id <> "'"
                  $(logError) msg
                  pure $ FailedSourceUpdate {id, msg}
                Just metadata -> do
                  let tags = PgJSONB $ Ty.tagsToValue (F.tags metadata)
                  let metadataFormat = coerce $ F.formatId metadata
                  let q =
                        runUpdate $
                          B.update
                            (DB.meloDb ^. #source)
                            ( \s ->
                                (s ^. #metadata <-. val_ tags)
                                  <> (s ^. #metadata_format <-. val_ metadataFormat)
                            )
                            (\s -> s ^. #id ==. val_ key)
                  $(runPgDebug') q
                  pure
                    UpdatedSource
                      { id = toText key
                      }
            Nothing -> do
              let id = update ^. #id
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
        setTagsFromMapped :: (F.TagMapping -> F.TagLens) -> F.Tags -> MappedTags INPUT -> F.Tags
        setTagsFromMapped tag ts MappedTags {..} = ts & tag M.artist .~ V.fromList (fromMaybe [] artistName)
    resolveMetadata (UpdateMappedTags m) metadata = metadata {F.tags = undefined}
    resolveMetadata (SetTags ps) metadata = metadata {F.tags = F.Tags $ V.fromList $ ps <&> \(UpdatePair k v) -> (k, v)}
    resolveMetadata (UpdateTags ps) metadata = metadata {F.tags = undefined}

data SourceUpdate' = SourceUpdate'
  { id :: Text,
    updateTags :: TagUpdateOp,
    originalSource :: DB.Source
  }
  deriving (Generic)

data TagUpdateOp
  = SetMappedTags (MappedTags INPUT)
  | UpdateMappedTags (MappedTags INPUT)
  | SetTags [UpdatePair]
  | UpdateTags [UpdatePair]

instance TryFrom TagUpdate TagUpdateOp where
  tryFrom TagUpdate {..} =
    SetMappedTags <$> setMappedTags
      <|> UpdateMappedTags <$> updateMappedTags
      <|> SetTags <$> setTags
      <|> UpdateTags <$> updateTags
