{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Service where

import Control.Applicative ((<|>))
import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Either.Combinators
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Data.Text.Lens
import Database.PostgreSQL.Simple (Connection)
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Uri
import qualified Melo.Database.Model as DB
import qualified Melo.Format as F
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo as Repo
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import System.FilePath
import Text.Printf

class Monad m => CollectionService m where
  addCollection :: NewCollection -> m CollectionRef
  deleteCollection :: CollectionRef -> m ()
  rescanCollection :: CollectionRef -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    CollectionService m
  ) =>
  CollectionService (t m)
  where
  addCollection = lift . addCollection
  deleteCollection = lift . deleteCollection
  rescanCollection = lift . rescanCollection

newtype CollectionServiceT m a = CollectionServiceT
  { runCollectionServiceT :: ReaderT (Pool Connection) m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadCatch, MonadMask, MonadThrow, MonadIO, MonadConc, MonadTrans, MonadTransControl)

runCollectionServiceIO :: Pool Connection -> CollectionServiceT m a -> m a
runCollectionServiceIO pool = flip runReaderT pool . runCollectionServiceT

instance
  ( CollectionRepository m,
    FileSystemService m,
    FileSystemWatchService m,
    MonadConc m,
    MonadIO m,
    Logging m
  ) =>
  CollectionService (CollectionServiceT m)
  where
  addCollection c@NewFilesystemCollection {..} = CollectionServiceT $
    ReaderT $ \pool -> do
      $(logInfo) $ "Adding collection " <> name
      $(logDebug) $ "Adding collection " <> show c
      cs <- Repo.insertCollections [c]
      --      (pool, collectionWatchState) <- ask
      case cs of
        [DB.Collection {..}] -> do
          let ref = CollectionRef id
          forkFileSystemServiceIO pool $ scanPath ref (T.unpack rootPath)
          when watch $ startWatching ref (T.unpack rootPath)
          pure ref
        _otherwise -> error "unexpected insertCollections result"
  rescanCollection ref@(CollectionRef id) = do
    listToMaybe <$> getCollections [DB.CollectionKey id] >>= \case
      Just DB.Collection {id, root_uri} ->
        case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
            scanPath ref rootPath >> pure ()
          Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
      Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
    pure ()
  deleteCollection ref@(CollectionRef id) = do
    stopWatching ref
    deleteCollections [DB.CollectionKey id]
    pure ()

data SourceMoveError
  = FileSystemMoveError MoveError
  | PatternError
  | NoSuchSource
  | SourcePathError
  deriving (Show)

moveSourceWithPattern ::
  ( FileSystem m,
    SourceRepository m,
    CollectionRepository m,
    Logging m
  ) =>
  NonEmpty SourcePathPattern ->
  DB.SourceKey ->
  m (Either SourceMoveError URI)
moveSourceWithPattern pats ref =
  getSource ref >>= \case
    Just src@Source {ref, source} -> case uriToFilePath source of
      Just srcPath ->
        previewSourceMoveWithPattern pats src >>= \case
          Just destPath -> do
            let DB.SourceKey id = ref
            $(logInfo) $ "moving source " <> show id <> " from " <> srcPath <> " to " <> destPath
            r <- mapLeft FileSystemMoveError <$> movePath srcPath destPath
            $(logInfo) $ "successfully moved source " <> show id <> " from " <> srcPath <> " to " <> destPath
            pure $ r $> fileUri destPath
          Nothing -> pure $ Left PatternError
      Nothing -> pure $ Left SourcePathError
    Nothing -> pure $ Left NoSuchSource

previewSourceKeyMoveWithPattern ::
  ( SourceRepository m,
    CollectionRepository m
  ) =>
  NonEmpty SourcePathPattern ->
  DB.SourceKey ->
  m (Maybe FilePath)
previewSourceKeyMoveWithPattern pats ref =
  getSource ref >>= \case
    Just Source {metadata, collectionRef, source} ->
      getCollections [collectionRef] >>= \case
        [DB.Collection {root_uri}] -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
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
  getCollections [collectionRef] >>= \case
    [DB.Collection {root_uri}] -> case parseURI (T.unpack root_uri) >>= uriToFilePath of
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
  GroupPattern pats -> Just $ fromMaybe "" (foldMap (renderSourcePattern metadata) pats)
  MappingPattern mapping -> tags ^? lens mapping . _head . unpacked
  DefaultPattern a b -> renderSourcePattern metadata a <|> renderSourcePattern metadata b
  PrintfPattern fmt pat ->
    printf fmt <$> renderSourcePattern metadata pat

data SourcePathPattern
  = LiteralPattern FilePath
  | GroupPattern (NonEmpty SourcePathPattern)
  | MappingPattern F.TagMapping
  | DefaultPattern SourcePathPattern SourcePathPattern
  | PrintfPattern String SourcePathPattern
