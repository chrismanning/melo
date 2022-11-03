{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform where

import Control.Applicative hiding (many, some)
import Control.Concurrent.Classy
import Control.Exception.Safe as E hiding (try)
import Control.Foldl (PrimMonad)
import Control.Foldl qualified as Fold
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Either.Combinators
import Data.Foldable
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty hiding (length)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
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
import Melo.Library.Album.ArtistName.Repo
import Melo.Library.Album.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types
import Melo.Library.Source.MultiTrack
import Melo.Library.Source.Repo as Src
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Service
import Melo.Metadata.Mapping.Types
import Rel8 qualified (JSONBEncoded (..))
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
evalTransformAction t _ = pure $ Left (UnsupportedTransform (show t))

type MonadSourceTransform m =
  ( CollectionRepository m,
    FileSystem m,
    FileSystemWatchService m,
    Logging m,
    MetadataService m,
    Monad m,
    MonadCatch m,
    MonadConc m,
    PrimMonad m,
    MultiTrack m,
    MusicBrainzService m,
    SourceRepository m,
    TagMappingRepository m,
    AlbumRepository m,
    AlbumArtistNameRepository m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackRepository m,
    TrackArtistNameRepository m
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

instance MonadSourceTransform m => MetadataService (TransformPreviewT m) where
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
  delete _ = pure ()
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

data TransformationError
  = MoveTransformError SourceMoveError
  | MetadataTransformError F.MetadataException
  | UnknownTagMapping Text
  | UnsupportedSourceKind
  | CollectionNotFound CollectionRef
  | SourceNotFound SourceRef
  | ImportFailed
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
    TagMappingRepository m,
    FileSystemWatchService m,
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
    FileSystemWatchService m,
    ArtistRepository m,
    Logging m
  ) =>
  Maybe CollectionRef ->
  NonEmpty SourcePathPattern ->
  SourceRef ->
  m (Either SourceMoveError Source)
moveSourceAtRefWithPattern collectionRef pats ref =
  getSource ref >>= \case
    Just src -> moveSourceWithPattern collectionRef pats src
    Nothing -> pure $ Left NoSuchSource

previewSourceKeyMoveWithPattern ::
  ( SourceRepository m,
    CollectionRepository m,
    TagMappingRepository m,
    ArtistRepository m,
    Logging m
  ) =>
  Maybe CollectionRef ->
  NonEmpty SourcePathPattern ->
  SourceRef ->
  m (Maybe FilePath)
previewSourceKeyMoveWithPattern collectionRef pats ref =
  getSource ref >>= \case
    Just src ->
      previewSourceMoveWithPattern (fromMaybe src.collectionRef collectionRef) pats src
    Nothing -> pure Nothing

previewSourceMoveWithPattern ::
  ( CollectionRepository m,
    TagMappingRepository m,
    ArtistRepository m,
    Logging m
  ) =>
  CollectionRef ->
  NonEmpty SourcePathPattern ->
  Source ->
  m (Maybe FilePath)
previewSourceMoveWithPattern collectionRef pats src@Source {source} =
  Repo.getByKey @Collection (V.singleton collectionRef) <&> firstOf traverse >>= \case
    Just CollectionTable {root_uri} -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
      Just rootPath -> do
        mappings <- Repo.getAll
        srcPath <- renderSourcePath mappings rootPath src pats
        pure $ Just $ srcPath <> takeExtension (show source)
      Nothing -> pure Nothing
    Nothing -> pure Nothing

renderSourcePath ::
  ( ArtistRepository m,
    Logging m
  ) =>
  Vector TagMappingEntity ->
  FilePath ->
  Source ->
  NonEmpty SourcePathPattern ->
  m FilePath
renderSourcePath mappings basepath src pats =
  let (<</>>) = liftM2 (</>)
   in pure basepath
        <</>> fromMaybe ""
        <$> Fold.foldM (Fold.sink (renderSourcePattern mappings src)) pats

renderSourcePattern ::
  ( ArtistRepository m,
    Logging m
  ) =>
  Vector TagMappingEntity ->
  Source ->
  SourcePathPattern ->
  m (Maybe FilePath)
renderSourcePattern mappings src = \case
  LiteralPattern p -> pure $ Just p
  GroupPattern pats -> do
    $(logDebug) $ "GroupPattern " <> show pats
    ts <- forM pats (renderSourcePattern mappings src)
    $(logDebug) $ "ts: " <> show ts
    let x = foldl' appendJust (Just "") ts
    $(logDebug) $ "x: " <> show x
    pure $ Just (fromMaybe "" x)
  MappingPattern mappingName -> runMaybeT do
    tagMapping <- MaybeT $ pure $ findMappingNamed mappings mappingName
    let e = TagMappingTable {name = mappingName, field_mappings = Rel8.JSONBEncoded tagMapping}
    rs <- resolveMapping e src
    MaybeT $ pure $ T.unpack <$> formatList (V.toList rs)
  DefaultPattern a b -> renderSourcePattern mappings src a <<|>> renderSourcePattern mappings src b
  PrintfPattern fmt pat ->
    fmap (printf fmt) <$> renderSourcePattern mappings src pat
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
extractTrack collectionRef' patterns s@Source {multiTrack = Just MultiTrackDesc {..}} =
  let collectionRef = fromMaybe s.collectionRef collectionRef'
   in uriToFilePath s.source & \case
        Just filePath ->
          Repo.getSingle @Collection collectionRef >>= \case
            Nothing -> pure $ Left (CollectionNotFound collectionRef)
            Just CollectionTable {root_uri} -> case uriToFilePath =<< parseURI (T.unpack root_uri) of
              Nothing -> pure $ Left UnsupportedSourceKind
              Just basePath -> do
                mappings <- Repo.getAll
                srcPath <- renderSourcePath mappings basePath s patterns
                let dest = addExtension srcPath (takeExtension filePath)
                lockPathsDuring (dest :| []) $ runExceptT do
                  mf <- openMetadataFile filePath >>= mapE MetadataTransformError
                  let cuefile =
                        CueFileSource
                          { idx,
                            range,
                            filePath,
                            metadata = s.metadata,
                            audioInfo = mf.audioInfo,
                            fileId = s.kind,
                            cueFilePath = filePath,
                            pictures = mf.pictures
                          }
                  raw <- extractTrackTo cuefile dest >>= mapE MultiTrackError
                  let mappings' = mappings <&> \m -> Rel8.fromJSONBEncoded m.field_mappings
                  $(logDebug) $ "Mappings: " <> show mappings'
                  let vc = fromMaybe (F.metadataFactory @F.VorbisComments F.emptyTags) $ F.convert' F.vorbisCommentsId s.metadata mappings'
                  $(logDebug) $ "Original metadata: " <> show s.metadata
                  $(logDebug) $ "Converted metadata: " <> show vc
                  let m = H.fromList [(F.vorbisCommentsId, vc)]
                  let metadataFile = raw & #metadata .~ m
                  mf <- writeMetadataFile metadataFile dest >>= mapE MetadataTransformError
                  srcs <- importSources (V.singleton (FileSource collectionRef mf))
                  if V.null srcs
                    then throwE ImportFailed
                    else pure $ srcs V.! 0
        Nothing -> pure $ Right s
  where
    mapE e = except . mapLeft e
extractTrack _ _ src = pure $ Right src

data MetadataTransformation
  = SetMapping Text [Text]
  | RemoveMappings [Text]
  | Retain [Text]
  deriving (Show)

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
