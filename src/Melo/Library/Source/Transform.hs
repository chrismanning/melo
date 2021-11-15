{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Transform where

import Control.Applicative
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Conc.Class
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.Either.Combinators
import Data.Foldable
import Data.List.NonEmpty
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lens
import Melo.Common.FileSystem as FS
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import qualified Melo.Database.Repo as Repo
import Melo.Format as F
import qualified Melo.Format.Mapping as M
import Melo.Lookup.MusicBrainz
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import System.FilePath
import System.IO (TextEncoding)
import Text.Printf
import Witch

type Transform m = [Source] -> m [Source]

data TransformAction where
  Move :: NonEmpty SourcePathPattern -> TransformAction
  ExtractEmbeddedImage :: URI -> TransformAction
  MoveCoverImage :: URI -> URI -> TransformAction
  SplitMultiTrackFile :: NonEmpty SourcePathPattern -> TransformAction
  RemoveOtherFiles :: TransformAction
  MusicBrainzLookup :: TransformAction
  ConvertEncoding :: TextEncoding -> TransformAction
  EditMetadata :: MetadataTransformation -> TransformAction

evalTransformActions :: MonadSourceTransform m => [TransformAction] -> Transform m
evalTransformActions = foldl' (\b a -> b >=> evalTransformAction a) pure

evalTransformAction :: MonadSourceTransform m => TransformAction -> Transform m
evalTransformAction (Move patterns) srcs = do
  forM srcs $ \src -> do
    moveSourceWithPattern patterns src >>= \case
      Right movedSrc -> pure movedSrc
      Left e -> do
        $(logError) $ show e
        pure src
evalTransformAction (EditMetadata metadataTransformation) srcs = undefined

type MonadSourceTransform m = (
    Monad m,
    FileSystem m,
    SourceRepository m,
    CollectionRepository m,
    MusicBrainzService m,
    MetadataService m,
    Logging m
  )

applyTransformations :: MonadSourceTransform m => [Transform m] -> [Source] -> m [Source]
applyTransformations transformations srcs = foldM transform srcs transformations
  where
    transform :: [Source] -> Transform m -> m [Source]
    transform srcs f = f srcs

previewTransformations :: MonadSourceTransform m => [Transform (FileSystemPreviewT m)] -> [Source] -> m [Source]
previewTransformations transformations srcs = runFileSystemPreviewT $ foldM previewTransform srcs transformations
  where
    previewTransform srcs f = f srcs

newtype FileSystemPreviewT m a = FileSystemPreviewT
  { runFileSystemPreviewT :: m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadThrow, MonadMask)
  deriving (MonadTrans) via IdentityT

instance MonadSourceTransform m => FileSystem (FileSystemPreviewT m) where
  doesFileExist = lift . doesFileExist
  doesDirectoryExist = lift . doesDirectoryExist
  listDirectory = lift . listDirectory
  canonicalizePath = lift . canonicalizePath
  readFile = lift . FS.readFile
  movePath _ _ = lift $ do
    $(logDebug) ("Mocked movePath called" :: String)
    pure $ Right ()

data TransformationError = MoveTransformError SourceMoveError
  deriving (Show)

instance From SourceMoveError TransformationError where
  from = MoveTransformError

data MetadataTransformation =
    SetMapping M.TagMapping [Text]
  | RemoveMapping M.TagMapping
  | Retain [M.TagMapping]

moveSourceWithPattern ::
  ( FileSystem m,
    CollectionRepository m,
    SourceRepository m,
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
          r <- mapLeft FileSystemMoveError <$> movePath srcPath destPath
          let newUri = fileUri destPath
          case r of
            Right _ -> do
              $(logInfo) $ "successfully moved source " <> show id <> " from " <> srcPath <> " to " <> destPath
              let movedSrc = src & #source .~ newUri
              Repo.update @SourceEntity [from movedSrc] >>= \case
                [updatedSrc] -> pure $ mapLeft ConversionError $ tryFrom updatedSrc
                _ -> pure $ Left SourceUpdateError
            Left e -> pure $ Left e
        Nothing -> pure $ Left PatternError
    Nothing -> pure $ Left SourcePathError

moveSourceAtRefWithPattern ::
  ( FileSystem m,
    SourceRepository m,
    CollectionRepository m,
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
    CollectionRepository m
  ) =>
  NonEmpty SourcePathPattern ->
  SourceRef ->
  m (Maybe FilePath)
previewSourceKeyMoveWithPattern pats ref =
  getSource ref >>= \case
    Just Source {metadata, collectionRef, source} ->
      getCollectionsByKey [collectionRef] >>= \case
        [CollectionTable {root_uri}] -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> pure $ Just $ renderSourcePatterns rootPath metadata pats <> takeExtension (show source)
          Nothing -> pure Nothing
        _ -> pure Nothing
    Nothing -> pure Nothing

previewSourceMoveWithPattern ::
  ( CollectionRepository m
  ) =>
  NonEmpty SourcePathPattern ->
  Source ->
  m (Maybe FilePath)
previewSourceMoveWithPattern pats Source {metadata, collectionRef, source} =
  Repo.getByKey [collectionRef] >>= \case
    [CollectionTable {root_uri}] -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
      Just rootPath -> pure $ Just $ renderSourcePatterns rootPath metadata pats <> takeExtension (show source)
      Nothing -> pure Nothing
    _ -> pure Nothing

renderSourcePatterns :: FilePath -> F.Metadata -> NonEmpty SourcePathPattern -> FilePath
renderSourcePatterns basepath metadata pats =
  basepath
    </> fromMaybe "" (foldMap (renderSourcePattern metadata) pats)

renderSourcePattern :: F.Metadata -> SourcePathPattern -> Maybe FilePath
renderSourcePattern metadata@F.Metadata {..} = \case
  LiteralPattern p -> Just p
  GroupPattern pats -> Just $ fromMaybe "" (foldl' (\s pat -> liftA2 mappend s $ renderSourcePattern metadata pat) (Just "") pats)
  MappingPattern mapping -> tags ^? lens mapping . _head . unpacked
  DefaultPattern a b -> renderSourcePattern metadata a <|> renderSourcePattern metadata b
  PrintfPattern fmt pat ->
    printf fmt <$> renderSourcePattern metadata pat
