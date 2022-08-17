{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform where

import Control.Applicative hiding (many, some)
import Control.Exception.Safe hiding (try)
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch hiding (try)
import Control.Monad.Conc.Class
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Char
import Data.Coerce
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import Data.Either.Combinators
import Data.Foldable
import Data.HashMap.Strict qualified as H
import Data.Int
import Data.List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Range
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lens
import Data.Time
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Melo.Common.FileSystem as FS
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo qualified as Repo
import Melo.Format qualified as F
import Melo.Format.Error qualified as F
import Melo.Format.Internal.Metadata (Metadata (..))
import Melo.Format.Mapping qualified as M
import Melo.Format.Metadata qualified as F
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo as Src
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import Melo.Lookup.MusicBrainz
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Service
import Melo.Metadata.Mapping.Types
import Sound.File.Sndfile qualified as Sndfile
import System.Directory qualified as Dir
import System.FilePath
import System.IO (TextEncoding)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf
import Witch

type Transform m = Vector Source -> m (Vector Source)

data TransformAction where
  Move :: NonEmpty SourcePathPattern -> TransformAction
  ExtractEmbeddedImage :: URI -> TransformAction
  MoveCoverImage :: URI -> URI -> TransformAction
  SplitMultiTrackFile :: NonEmpty SourcePathPattern -> TransformAction
  RemoveOtherFiles :: TransformAction
  MusicBrainzLookup :: TransformAction
  ConvertEncoding :: TextEncoding -> TransformAction
  EditMetadata :: MetadataTransformation -> TransformAction

evalTransformActions :: MonadSourceTransform m => Vector TransformAction -> Transform m
evalTransformActions = foldl' (\b a -> b >=> evalTransformAction a) pure

evalTransformAction :: MonadSourceTransform m => TransformAction -> Transform m
evalTransformAction (Move patterns) srcs = transformSources (moveSourceWithPattern patterns) srcs
evalTransformAction (SplitMultiTrackFile patterns) srcs = transformSources (extractTrack patterns) srcs
evalTransformAction (EditMetadata metadataTransformation) srcs = transformSources (editMetadata metadataTransformation) srcs
evalTransformAction _ _ = error "unimplemented transformation"

transformSources :: (Logging m, Show e) => (a -> m (Either e a)) -> Vector a -> m (Vector a)
transformSources t srcs = forM srcs $ \src ->
  t src >>= \case
    Right transformedSrc -> pure transformedSrc
    Left e -> do
      $(logError) $ show e
      pure src

type MonadSourceTransform m =
  ( Monad m,
    FileSystem m,
    SourceRepository m,
    CollectionRepository m,
    MusicBrainzService m,
    MetadataService m,
    TagMappingRepository m,
    MonadIO m,
    Logging m
  )

applyTransformations :: MonadSourceTransform m => [Transform m] -> Vector Source -> m (Vector Source)
applyTransformations transformations srcs = foldM transform srcs transformations
  where
    transform srcs f = f srcs

previewTransformations ::
  MonadSourceTransform m =>
  [Transform (TransformPreviewT m)] ->
  Vector Source ->
  m (Vector Source)
previewTransformations transformations srcs =
  runTransformPreviewT $
    foldM previewTransform srcs transformations
  where
    previewTransform srcs f = f srcs

newtype TransformPreviewT m a = TransformPreviewT
  { runTransformPreviewT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask)
  deriving (MonadTrans) via IdentityT
  deriving (SourceRepository)

instance MonadSourceTransform m => FileSystem (TransformPreviewT m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile = lift . FS.readFile
  movePath _ _ = lift $ do
    $(logDebug) ("Preview movePath called" :: String)
    pure $ Right ()

instance MonadSourceTransform m => MetadataService (TransformPreviewT m) where
  openMetadataFile = lift . openMetadataFile
  openMetadataFileByExt = lift . openMetadataFileByExt
  readMetadataFile mid p = lift $ readMetadataFile mid p
  writeMetadataFile mf _p = lift $ do
    $(logDebug) ("Preview writeMetadataFile called" :: String)
    pure $ Right mf

instance MonadSourceTransform m => Repo.Repository SourceEntity (TransformPreviewT m) where
  getAll = lift $ Repo.getAll @SourceEntity
  getByKey = lift . Repo.getByKey @SourceEntity
  insert = pure . fmap from
  insert' _ = pure ()
  delete _ = pure ()
  update e = lift $ do
    $(logDebug) ("Preview update @SourceEntity called" :: String)
    pure e
  update' _ = lift $ do
    $(logDebug) ("Preview update' @SourceEntity called" :: String)
    pure ()

data TransformationError
  = MoveTransformError SourceMoveError
  | MetadataTransformError F.MetadataException
  | UnknownTagMapping Text
  | UnsupportedSourceKind
  | CollectionNotFound CollectionRef
  | SourceNotFound SourceRef
  | ImportFailed
  deriving (Show)

instance From SourceMoveError TransformationError where
  from = MoveTransformError

instance From F.MetadataException TransformationError where
  from = MetadataTransformError

moveSourceWithPattern ::
  ( FileSystem m,
    CollectionRepository m,
    SourceRepository m,
    TagMappingRepository m,
    Logging m
  ) =>
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either SourceMoveError Source)
moveSourceWithPattern pats src@Source {ref, source} =
  case uriToFilePath source of
    Just srcPath ->
      previewSourceMoveWithPattern pats src >>= \case
        Just destPath -> do
          let SourceRef id = ref
          $(logInfo) $ "moving source " <> show id <> " from " <> srcPath <> " to " <> destPath
          -- TODO ignore filesystem events involved - remove, import explicitly
          r <- mapLeft from <$> movePath srcPath destPath
          let newUri = fileUri destPath
          case r of
            Right _ -> do
              $(logInfo) $ "successfully moved source " <> show id <> " from " <> srcPath <> " to " <> destPath
              let movedSrc = src & #source .~ newUri
              Repo.update @SourceEntity (V.singleton (from movedSrc)) <&> firstOf traverse >>= \case
                Just updatedSrc -> pure $ mapLeft ConversionError $ tryFrom updatedSrc
                Nothing -> pure $ Left SourceUpdateError
            Left e -> pure $ Left e
        Nothing -> pure $ Left PatternError
    Nothing -> pure $ Left SourcePathError

moveSourceAtRefWithPattern ::
  ( FileSystem m,
    SourceRepository m,
    CollectionRepository m,
    TagMappingRepository m,
    Logging m
  ) =>
  NonEmpty SourcePathPattern ->
  SourceRef ->
  m (Either SourceMoveError Source)
moveSourceAtRefWithPattern pats ref =
  getSource ref >>= \case
    Just src -> moveSourceWithPattern pats src
    Nothing -> pure $ Left NoSuchSource

previewSourceKeyMoveWithPattern ::
  ( SourceRepository m,
    CollectionRepository m,
    TagMappingRepository m
  ) =>
  NonEmpty SourcePathPattern ->
  SourceRef ->
  m (Maybe FilePath)
previewSourceKeyMoveWithPattern pats ref =
  getSource ref >>= \case
    Just Source {metadata, collectionRef, source} ->
      getCollectionsByKey (V.singleton collectionRef) <&> firstOf traverse >>= \case
        Just CollectionTable {root_uri} -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            mappings <- Repo.getAll
            pure $ Just $ renderSourcePatterns mappings rootPath metadata pats <> takeExtension (show source)
          Nothing -> pure Nothing
        Nothing -> pure Nothing
    Nothing -> pure Nothing

previewSourceMoveWithPattern ::
  ( CollectionRepository m,
    TagMappingRepository m
  ) =>
  NonEmpty SourcePathPattern ->
  Source ->
  m (Maybe FilePath)
previewSourceMoveWithPattern pats Source {metadata, collectionRef, source} =
  Repo.getByKey @Collection (V.singleton collectionRef) <&> firstOf traverse >>= \case
    Just CollectionTable {root_uri} -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
      Just rootPath -> do
        mappings <- Repo.getAll
        pure $ Just $ renderSourcePatterns mappings rootPath metadata pats <> takeExtension (show source)
      Nothing -> pure Nothing
    Nothing -> pure Nothing

renderSourcePatterns :: Vector TagMappingEntity -> FilePath -> F.Metadata -> NonEmpty SourcePathPattern -> FilePath
renderSourcePatterns mappings basepath metadata pats =
  basepath
    </> fromMaybe "" (foldMap (renderSourcePattern mappings metadata) pats)

renderSourcePattern :: Vector TagMappingEntity -> F.Metadata -> SourcePathPattern -> Maybe FilePath
renderSourcePattern mappings metadata@F.Metadata {..} = \case
  LiteralPattern p -> Just p
  GroupPattern pats -> Just $ fromMaybe "" (foldl' (\s pat -> liftA2 mappend s $ renderSourcePattern mappings metadata pat) (Just "") pats)
  MappingPattern mappingName -> do
    mapping <- findMappingNamed mappings mappingName
    tags ^? lens mapping . _head . unpacked
  DefaultPattern a b -> renderSourcePattern mappings metadata a <|> renderSourcePattern mappings metadata b
  PrintfPattern fmt pat ->
    printf fmt <$> renderSourcePattern mappings metadata pat

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
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either F.MetadataException Source)
extractTrack patterns s@Source {multiTrack = Just MultiTrackDesc {..}, ..} =
  uriToFilePath source & \case
    Just filePath ->
      Repo.getSingle @Collection collectionRef >>= \case
        Nothing -> pure $ Left F.UnsupportedFormat
        Just CollectionTable {root_uri} -> case uriToFilePath =<< parseURI (T.unpack root_uri) of
          Nothing -> pure $ Left F.UnsupportedFormat
          Just basePath -> do
            mappings <- Repo.getAll
            let dest = addExtension (renderSourcePatterns mappings basePath metadata patterns) (takeExtension filePath)
            liftIO $ do
              Dir.createDirectoryIfMissing True (takeDirectory dest)
              let start = getStartTime range
              src <- sourceSndFrom @_ @Int16 start filePath
              info <- Sndfile.getFileInfo filePath
              let src' = case getEndTime range of
                    Just end -> takeStart end src
                    Nothing -> src
              runResourceT $ sinkSnd dest (Sndfile.format info) src'
            openMetadataFile dest >>= \case
              Right raw -> runExceptT do
                let vc = fromMaybe (F.metadataFactory @F.VorbisComments F.emptyTags) $ F.convert F.vorbisCommentsId metadata
                let m = H.fromList [(F.vorbisCommentsId, vc)]
                let metadataFile = raw & #metadata .~ m
                mf <- writeMetadataFile metadataFile dest >>= eitherToError
                importSources (V.singleton (FileSource collectionRef mf)) >>= headOrException F.UnsupportedFormat
              Left e -> pure $ Left e
    Nothing -> pure $ Right s
  where
    getStartTime :: AudioRange -> Duration
    getStartTime (TimeRange (SpanRange lower _upper)) = Seconds $ toSeconds $ boundValue lower
    getStartTime (TimeRange (LowerBoundRange lower)) = Seconds $ toSeconds $ boundValue lower
    getStartTime (TimeRange _) = Seconds 0
    getStartTime (SampleRange _range) = Frames 0
    getEndTime :: AudioRange -> Maybe Duration
    getEndTime (TimeRange (SpanRange lower upper)) = Just $ Seconds $ toSeconds (boundValue upper) - toSeconds (boundValue lower)
    getEndTime (TimeRange (LowerBoundRange _)) = Nothing
    getEndTime (TimeRange _) = Nothing
    getEndTime (SampleRange _) = Nothing
    toSeconds = realToFrac . nominalDiffTimeToSeconds . ctTime
extractTrack _ src = pure $ Right src

data MetadataTransformation
  = SetMapping Text [Text]
  | RemoveMappings [Text]
  | Retain [Text]

editMetadata :: MonadSourceTransform m => MetadataTransformation -> Source -> m (Either TransformationError Source)
editMetadata (SetMapping mappingName vs) Source {metadata = m@Metadata {tags, lens = tag}, ..} =
  case uriToFilePath source of
    Nothing -> pure $ Left UnsupportedSourceKind
    Just path -> runExceptT @TransformationError do
      mappings <- Repo.getAll
      mapping <- eitherToError $ maybeToRight (UnknownTagMapping mappingName) $ findMappingNamed mappings mappingName
      let newMetadata@Metadata {formatId} :: Metadata = m {F.tags = tags & tag mapping .~ V.fromList vs}
      raw <- eitherToError . mapLeft from =<< readMetadataFile kind path
      let metadataFile = raw & #metadata . at formatId .~ Just newMetadata
      metadataFile' <- eitherToError . mapLeft from =<< writeMetadataFile metadataFile path
      importSources (V.singleton (FileSource collectionRef metadataFile')) >>= headOrException ImportFailed
editMetadata (RemoveMappings mappings) src = flatten <$> (forM mappings $ \mapping -> editMetadata (SetMapping mapping []) src)
  where
    flatten :: [Either TransformationError Source] -> Either TransformationError Source
    flatten es = foldl' (\_a b -> b) (Left ImportFailed) es
editMetadata (Retain retained) Source {metadata = m@Metadata {tags, lens = tag}, ..} =
  case uriToFilePath source of
    Nothing -> pure $ Left UnsupportedSourceKind
    Just path -> runExceptT @TransformationError do
      mappings <- Repo.getAll
      retainedMappings <- forM retained $ \mappingName ->
        eitherToError $
          maybeToRight (UnknownTagMapping mappingName) $
            findMappingNamed mappings mappingName
      let newTags = foldl' (\ts mapping -> ts & tag mapping .~ (tags ^. tag mapping)) F.emptyTags retainedMappings
      let newMetadata@Metadata {formatId} :: Metadata = m {F.tags = newTags}
      raw <- eitherToError . mapLeft from =<< readMetadataFile kind path
      let metadataFile = raw & #metadata . at formatId .~ Just newMetadata
      metadataFile' <- eitherToError . mapLeft from =<< writeMetadataFile metadataFile path
      importSources (V.singleton (FileSource collectionRef metadataFile')) >>= headOrException ImportFailed

headOrException :: (Traversable f, MonadError e m) => e -> f a -> m a
headOrException e xs = case firstOf traverse xs of
  Just x -> pure x
  Nothing -> throwError e
