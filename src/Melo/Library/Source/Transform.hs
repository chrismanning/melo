{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform where

import Control.Applicative hiding (many, some)
import Control.Concurrent.Classy
import Control.Exception.Safe as E hiding (try)
import Control.Foldl qualified as Fold
import Control.Lens hiding (from)
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Data.Char
import Data.Either.Combinators
import Data.Foldable
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty hiding (length)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Melo.Common.FileSystem as FS
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Database.Repo qualified as Repo
import Melo.Format qualified as F
import Melo.Format.Error qualified as F
import Melo.Format.Internal.Metadata (Metadata (..))
import Melo.Format.Mapping qualified as M
import Melo.Library.Album.Aggregate
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Repo
import Melo.Library.Collection.FileSystem.Watcher
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types
import Melo.Library.Source.Aggregate
import Melo.Library.Source.MultiTrack
import Melo.Library.Source.Repo as Src
import Melo.Library.Source.Types
import Melo.Library.Track.Aggregate
import Melo.Lookup.MusicBrainz as MB
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import System.FilePath
import System.IO (TextEncoding)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf
import Witch

type Transform m = Source -> m (Either TransformationError Source)

data TransformAction where
  Move :: Maybe CollectionRef -> NonEmpty SourcePathPattern -> TransformAction
  Copy :: Maybe CollectionRef -> NonEmpty SourcePathPattern -> TransformAction
  ExtractEmbeddedImage :: URI -> TransformAction
  EmbedImage :: URI -> TransformAction
  MoveCoverImage :: URI -> URI -> TransformAction
  SplitMultiTrackFile :: Maybe CollectionRef -> NonEmpty SourcePathPattern -> TransformAction
  RemoveOtherFiles :: TransformAction
  MusicBrainzLookup :: TransformAction
  ConvertEncoding :: TextEncoding -> TransformAction
  EditMetadata :: MetadataTransformation -> TransformAction
  ConvertMetadataFormat :: F.MetadataId -> TransformAction
  ConvertFileFormat :: F.MetadataFileId -> TransformAction

deriving instance Show TransformAction

evalTransformActions :: MonadSourceTransform m => Vector TransformAction -> Source -> m (Either TransformationError Source)
evalTransformActions = foldl' (\b a -> b >=/> evalTransformAction a) (pure . Right)

(>=/>) :: Monad m => Transform m -> Transform m -> Transform m
f >=/> g = \x ->
  f x >>= \case
    Left e -> pure $ Left e
    Right s -> g s

evalTransformAction :: MonadSourceTransform m => TransformAction -> Transform m
evalTransformAction (Move ref patterns) src = mapLeft from <$> moveSourceWithPattern ref patterns src
evalTransformAction (SplitMultiTrackFile ref patterns) src = mapLeft from <$> extractTrack ref patterns src
evalTransformAction (EditMetadata metadataTransformation) src = mapLeft from <$> editMetadata metadataTransformation src
evalTransformAction MusicBrainzLookup src = musicBrainzLookup src
evalTransformAction t _ = pure $ Left (UnsupportedTransform (show t))

type MonadSourceTransform m =
  ( CollectionRepository m,
    FileSystem m,
    FileSystemWatcher m,
    Logging m,
    MetadataAggregate m,
    Monad m,
    MonadCatch m,
    MonadConc m,
    PrimMonad m,
    MultiTrack m,
    MusicBrainzService m,
    SourceAggregate m,
    AlbumAggregate m,
    ArtistAggregate m,
    TrackAggregate m,
    SourceRepository m,
    TagMappingRepository m,
    TagMappingAggregate m,
    ArtistRepository m
  )

previewTransformation ::
  MonadSourceTransform m =>
  Transform (TransformPreviewT m) ->
  Source ->
  m (Either TransformationError Source)
previewTransformation transformation src =
  runTransformPreviewT $
    transformation src

previewTransformations ::
  MonadSourceTransform m =>
  Transform (TransformPreviewT m) ->
  Vector Source ->
  m (Vector (Either TransformationError Source))
previewTransformations transformation srcs =
  runTransformPreviewT $
    mapM transformation srcs

newtype TransformPreviewT m a = TransformPreviewT
  { runTransformPreviewT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask, PrimMonad)
  deriving (MonadTrans, MonadTransControl) via IdentityT
  deriving (SourceRepository)

instance MonadSourceTransform m => FileSystem (TransformPreviewT m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile p = lift $ do
    $(logDebug) ("readFile called with " <> p)
    FS.readFile p
  movePath _ _ = lift $ do
    $(logDebug) ("Preview movePath called" :: String)
    pure $ Right ()

instance MonadSourceTransform m => MetadataAggregate (TransformPreviewT m) where
  openMetadataFile _p = do
    $(logDebug) ("Preview openMetadataFile called" :: String)
    lift $ openMetadataFile _p
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile mid p = lift $ readMetadataFile mid p
  writeMetadataFile mf _p = lift $ do
    $(logDebug) ("Preview writeMetadataFile called" :: String)
    pure $ Right mf

instance MonadSourceTransform m => Repo.Repository SourceEntity (TransformPreviewT m) where
  getAll = lift $ Repo.getAll @SourceEntity
  getByKey = lift . Repo.getByKey @SourceEntity
  insert = pure . fmap from
  insert' = pure . V.length
  delete _ = pure V.empty
  update e = lift $ do
    $(logDebug) ("Preview update @SourceEntity called" :: String)
    pure e
  update' _ = lift $ do
    $(logDebug) ("Preview update' @SourceEntity called" :: String)
    pure ()

instance MonadSourceTransform m => MultiTrack (TransformPreviewT m) where
  extractTrackTo cuefile dest =
    pure $
      Right
        F.MetadataFile
          { filePath = dest,
            metadata = H.empty,
            audioInfo = cuefile.audioInfo,
            fileId = cuefile.fileId,
            pictures = []
          }

instance MonadSourceTransform m => SourceAggregate (TransformPreviewT m) where
  importSources _ = do
    $(logDebug) ("Preview importSources called" :: String)
    pure empty
  updateSource s = do
    $(logDebug) ("Preview updateSource called" :: String)
    pure $ Right s

data TransformationError
  = MoveTransformError SourceMoveError
  | MetadataTransformError F.MetadataException
  | UnknownTagMapping Text
  | UnsupportedSourceKind
  | CollectionNotFound CollectionRef
  | SourceNotFound SourceRef
  | ImportFailed SourceError
  | MultiTrackError MultiTrackError
  | SourceConversionError (TryFromException SourceEntity Source)
  | UnsupportedTransform String
  | UnknownLength
  deriving (Show)

instance Exception TransformationError

instance From SourceMoveError TransformationError where
  from = MoveTransformError

instance From F.MetadataException TransformationError where
  from = MetadataTransformError

instance From (TryFromException SourceEntity Source) TransformationError where
  from = SourceConversionError

moveSourceWithPattern ::
  ( FileSystem m,
    CollectionRepository m,
    SourceRepository m,
    TagMappingAggregate m,
    FileSystemWatcher m,
    ArtistRepository m,
    Logging m
  ) =>
  Maybe CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either SourceMoveError Source)
moveSourceWithPattern collectionRef pats src@Source {ref, source} =
  case uriToFilePath source of
    Just srcPath ->
      previewSourceMoveWithPattern (fromMaybe src.collectionRef collectionRef) pats src >>= \case
        Just destPath | destPath == srcPath -> pure $ Right src
        Just destPath -> lockPathsDuring (srcPath :| [destPath]) do
          let SourceRef id = ref
          $(logInfo) $ "moving source " <> show id <> " from " <> srcPath <> " to " <> destPath
          movePath srcPath destPath >>= \case
            Left e -> pure $ Left (from e)
            Right _ -> do
              $(logInfo) $ "successfully moved source " <> show id <> " from " <> srcPath <> " to " <> destPath
              findCoverImage (takeDirectory srcPath) >>= \case
                Nothing -> pure ()
                Just imagePath -> do
                  let destImagePath = replaceDirectory imagePath (takeDirectory destPath)
                  movePath imagePath destImagePath >>= \case
                    Left e ->
                      $(logError) $
                        "failed to move cover image "
                          <> takeFileName imagePath
                          <> " for source "
                          <> show id
                          <> ": "
                          <> displayException e
                    Right _ ->
                      $(logInfo) $ "successfully moved cover image " <> takeFileName imagePath <> " for source " <> show id
              let movedSrc = src & #source .~ fileUri destPath
              Repo.updateSingle @SourceEntity (from movedSrc) >>= \case
                Just updatedSrc -> pure $ mapLeft ConversionError $ tryFrom updatedSrc
                Nothing -> pure $ Left SourceUpdateError
        Nothing -> pure $ Left PatternError
    Nothing -> pure $ Left SourcePathError

previewSourceMoveWithPattern ::
  ( CollectionRepository m,
    TagMappingAggregate m,
    ArtistRepository m,
    Logging m
  ) =>
  CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Maybe FilePath)
previewSourceMoveWithPattern collectionRef pats src@Source {source} =
  Repo.getSingle @CollectionEntity collectionRef >>= \case
    Just CollectionTable {root_uri} -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
      Just rootPath -> do
        srcPath <- renderSourcePath rootPath src pats
        pure $ Just $ srcPath <> takeExtension (show source)
      Nothing -> pure Nothing
    Nothing -> pure Nothing

renderSourcePath ::
  ( ArtistRepository m,
    TagMappingAggregate m,
    Logging m
  ) =>
  FilePath ->
  Source ->
  NonEmpty SourcePathPattern ->
  m FilePath
renderSourcePath basepath src pats =
  let (<</>>) = liftM2 (</>)
   in pure basepath
        <</>> fromMaybe ""
        <$> Fold.foldM (Fold.sink (renderSourcePattern src)) pats

renderSourcePattern ::
  ( ArtistRepository m,
    TagMappingAggregate m,
    Logging m
  ) =>
  Source ->
  SourcePathPattern ->
  m (Maybe FilePath)
renderSourcePattern src = \case
  LiteralPattern p -> pure $ Just p
  GroupPattern pats -> do
    ts <- forM pats (renderSourcePattern src)
    let x = foldl' appendJust (Just "") ts
    pure $ Just (fromMaybe "" x)
  MappingPattern mappingName -> do
    rs <- resolveMappingNamed mappingName src
    pure $ T.unpack <$> formatList (V.toList rs)
  DefaultPattern a b -> renderSourcePattern src a <<|>> renderSourcePattern src b
  PrintfPattern fmt pat ->
    fmap (printf fmt) <$> renderSourcePattern src pat
  where
    formatList :: [Text] -> Maybe Text
    formatList [] = Nothing
    formatList [a] = Just a
    formatList (a : [b]) = Just (a <> " & " <> b)
    formatList (a : as) = (\b -> a <> ", " <> b) <$> formatList as
    appendJust Nothing _ = Nothing
    appendJust _ Nothing = Nothing
    appendJust (Just a) (Just b) = Just (a <> b)

parseMovePattern :: Text -> Either (Maybe (ParseErrorBundle Text Void)) (NonEmpty SourcePathPattern)
parseMovePattern s = nonEmptyRight =<< mapLeft Just (parse terms "" s)
  where
    nonEmptyRight :: [SourcePathPattern] -> Either (Maybe (ParseErrorBundle Text Void)) (NonEmpty SourcePathPattern)
    nonEmptyRight (p : ps) = Right $ p :| ps
    nonEmptyRight [] = Left Nothing
    terms = someTill term eof
    term = try format <|> try group <|> try literal
    format = do
      void (char '%')
      padZero <- isJust <$> optional (char '0')
      width <- fmap (: []) <$> optional digitChar
      mapping' <- mapping
      case width of
        Just width' ->
          pure $
            PrintfPattern ("%" <> (if padZero then "0" else ".") <> width' <> "s") $
              MappingPattern mapping'
        Nothing -> pure $ MappingPattern mapping'
    mapping = T.pack <$> some (letterChar <|> char '_') <?> "mapping"
    group = do
      void (char '[')
      terms' <- someTill term (char ']')
      case nonEmpty terms' of
        Just terms'' -> pure $ GroupPattern terms''
        Nothing -> fail "no terms in group"
    literal =
      LiteralPattern
        <$> ( some
                ( satisfy @Void isAlphaNum
                    <|> satisfy @Void isSpace
                    <|> satisfy @Void (\c -> not (isReservedSymbol c) && isPunctuation c)
                )
                <?> "literal"
            )
    isReservedSymbol c = elem c ['[', ']', '%']

extractTrack ::
  MonadSourceTransform m =>
  Maybe CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either TransformationError Source)
extractTrack collectionRef' patterns s@Source {multiTrack = Just MultiTrackDesc {..}, metadata = Just metadata} =
  let collectionRef = fromMaybe s.collectionRef collectionRef'
   in uriToFilePath s.source & \case
        Just filePath ->
          Repo.getSingle @CollectionEntity collectionRef >>= \case
            Nothing -> pure $ Left (CollectionNotFound collectionRef)
            Just CollectionTable {root_uri} -> case uriToFilePath =<< parseURI (T.unpack root_uri) of
              Nothing -> pure $ Left UnsupportedSourceKind
              Just basePath -> do
                mappings <- V.fromList . Map.elems <$> getAllMappings
                srcPath <- renderSourcePath basePath s patterns
                let dest = addExtension srcPath (takeExtension filePath)
                lockPathsDuring (dest :| []) $ runExceptT do
                  mf <- openMetadataFile filePath >>= mapE MetadataTransformError
                  let cuefile =
                        CueFileSource
                          { idx,
                            range,
                            filePath,
                            metadata,
                            audioInfo = mf.audioInfo,
                            fileId = s.kind,
                            cueFilePath = filePath,
                            pictures = mf.pictures
                          }
                  raw <- extractTrackTo cuefile dest >>= mapE MultiTrackError
                  $(logDebug) $ "Mappings: " <> show mappings
                  let vc =
                        fromMaybe (F.metadataFactory @F.VorbisComments F.emptyTags) $
                          F.convert' F.vorbisCommentsId metadata mappings
                  $(logDebug) $ "Original metadata: " <> show metadata
                  $(logDebug) $ "Converted metadata: " <> show vc
                  let m = H.fromList [(F.vorbisCommentsId, vc)]
                  let metadataFile = raw & #metadata .~ m
                  mf <- writeMetadataFile metadataFile dest >>= mapE MetadataTransformError
                  srcs <- importSources (V.singleton (FileSource collectionRef mf))
                  if V.null srcs
                    then throwE (ImportFailed (ImportSourceError Nothing))
                    else pure $ srcs V.! 0
        Nothing -> pure $ Right s
extractTrack _ _ src = pure $ Right src

mapE :: Monad m => (b -> c) -> Either b a -> ExceptT c m a
mapE e = except . mapLeft e

data MetadataTransformation
  = SetMapping Text (Vector Text)
  | RemoveMappings (Vector Text)
  | Retain (Vector Text)
  deriving (Show)

editMetadata :: MonadSourceTransform m => MetadataTransformation -> Source -> m (Either TransformationError Source)
editMetadata _ src@Source {metadata = Nothing} = pure $ Right src
editMetadata (SetMapping mappingName vs) src@Source {metadata = Just metadata} =
  runExceptT @TransformationError do
    mapping <- getMappingNamed mappingName >>= eitherToError . maybeToRight (UnknownTagMapping mappingName)
    let newMetadata = metadata & F.tagLens mapping .~ vs
    if newMetadata == metadata
      then pure src
      else
        updateSource (src & #metadata .~ Just newMetadata) >>= mapE ImportFailed
editMetadata (RemoveMappings mappings) src = flatten <$> (forM mappings $ \mapping -> editMetadata (SetMapping mapping V.empty) src)
  where
    flatten es = foldl' (\_a b -> b) (Left $ ImportFailed undefined) es
editMetadata (Retain retained) src@Source {metadata = Just metadata} =
  let tag = F.mappedTag metadata.mappingSelector
   in runExceptT @TransformationError do
        retainedMappings <- forM retained $ \mappingName -> do
          getMappingNamed mappingName
            >>= eitherToError . maybeToRight (UnknownTagMapping mappingName)
        let newTags = foldl' (\ts mapping -> ts & tag mapping .~ (metadata.tag mapping)) F.emptyTags retainedMappings
        let newMetadata = metadata {F.tags = newTags}
        if newMetadata == metadata
          then pure src
          else
            updateSource (src & #metadata .~ Just newMetadata) >>= mapE ImportFailed

musicBrainzLookup :: MonadSourceTransform m => Source -> m (Either TransformationError Source)
musicBrainzLookup src@Source {metadata = Nothing} = do
  $(logDebug) $ "No Metadata; Cannot lookup MusicBrainz by metadata for source " <> show src.ref
  pure $ Right src
musicBrainzLookup src@Source {metadata = Just metadata} = (flip evalStateT) metadata do
  get >>= MB.getReleaseAndGroup >>= \case
    (Nothing, Nothing) -> pure ()
    (releaseGroup, Just release) -> do
      F.tagLens MB.releaseIdTag .= V.singleton release.id.mbid
      case releaseGroup ^? _Just . #id . coerced of
        Just releaseGroupId -> F.tagLens MB.releaseGroupIdTag .= V.singleton releaseGroupId
        _ -> pure ()
      let albumArtists = release.artistCredit ^.. _Just . traverse . (to (.artist.id.mbid))
      F.tagLens MB.albumArtistIdTag .= V.fromList albumArtists
      case release.labelInfo ^? _Just . traverse . to (.catalogNumber) . _Just of
        Just catNum -> F.tagLens M.catalogNumber .= V.singleton catNum
        _ -> pure ()
    (Just releaseGroup, Nothing) ->
      F.tagLens MB.releaseGroupIdTag .= V.singleton releaseGroup.id.mbid
  get >>= MB.getRecordingFromMetadata >>= \case
    Just recording -> do
      F.tagLens MB.recordingIdTag .= V.singleton recording.id.mbid
      let trackArtists = recording.artistCredit ^.. _Just . traverse . (to (.artist.id.mbid))
      F.tagLens MB.artistIdTag .= V.fromList trackArtists
    _ -> pure ()
  newMetadata <- get
  mapLeft ImportFailed <$> updateSource (src & #metadata .~ Just newMetadata)
