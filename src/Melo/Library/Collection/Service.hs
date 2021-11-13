{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Service where

import Control.Applicative ((<|>))
import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Either.Combinators
import Data.Foldable
import Data.Functor
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Data.Text.Lens
import Hasql.Connection
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Database.Repo as Repo
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

newtype CollectionServiceIOT m a = CollectionServiceIOT
  { runCollectionServiceIOT :: ReaderT (Pool Connection) m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadCatch,
      MonadConc,
      MonadMask,
      MonadParallel,
      MonadThrow,
      MonadReader (Pool Connection),
      MonadTrans,
      MonadTransControl
    )

runCollectionServiceIO :: Pool Connection -> CollectionServiceIOT m a -> m a
runCollectionServiceIO pool = flip runReaderT pool . runCollectionServiceIOT

instance
  ( CollectionRepository m,
    FileSystemService m,
    FileSystemWatchService m,
    MonadConc m,
    MonadParallel m,
    MonadIO m,
    Logging m
  ) =>
  CollectionService (CollectionServiceIOT m)
  where
  addCollection c@NewFilesystemCollection {..} = do
    pool <- ask
    $(logInfo) $ "Adding collection " <> name
    $(logDebug) $ "Adding collection " <> show c
    cs <- Repo.insert [c]
    --      (pool, collectionWatchState) <- ask
    case cs of
      [CollectionTable {..}] -> do
        forkFileSystemServiceIO pool $ scanPath id (T.unpack rootPath)
        when watch $ startWatching id (T.unpack rootPath)
        pure id
      _otherwise -> error "unexpected insertCollections result"
  rescanCollection ref@(CollectionRef id) = do
    listToMaybe <$> getCollectionsByKey [ref] >>= \case
      Just CollectionTable {id, root_uri} ->
        case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
            scanPath ref rootPath >> pure ()
          Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
      Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
    pure ()
  deleteCollection ref = do
    stopWatching ref
    delete [ref]
    pure ()

moveSourceWithPattern ::
  ( FileSystem m,
    CollectionRepository m,
    Logging m
  ) =>
  NonEmpty SourcePathPattern ->
  Source ->
  m (Either SourceMoveError URI)
moveSourceWithPattern pats src@Source {ref, source} =
  case uriToFilePath source of
    Just srcPath ->
      previewSourceMoveWithPattern pats src >>= \case
        Just destPath -> do
          let SourceRef id = ref
          $(logInfo) $ "moving source " <> show id <> " from " <> srcPath <> " to " <> destPath
          -- TODO ignore filesystem events involved - remove, import explicitly
          r <- mapLeft FileSystemMoveError <$> movePath srcPath destPath
          $(logInfo) $ "successfully moved source " <> show id <> " from " <> srcPath <> " to " <> destPath
          pure $ r $> fileUri destPath
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
  m (Either SourceMoveError URI)
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

getCollectionsByKey :: (CollectionRepository m) => [CollectionRef] -> m [Collection]
getCollectionsByKey = getByKey

previewSourceMoveWithPattern ::
  ( CollectionRepository m
  ) =>
  NonEmpty SourcePathPattern ->
  Source ->
  m (Maybe FilePath)
previewSourceMoveWithPattern pats Source {metadata, collectionRef, source} =
  getByKey [collectionRef] >>= \case
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
  GroupPattern pats -> Just $ fromMaybe "" (foldl' (\s pat -> s `mappendMaybe` renderSourcePattern metadata pat) (Just "") pats)
  MappingPattern mapping -> tags ^? lens mapping . _head . unpacked
  DefaultPattern a b -> renderSourcePattern metadata a <|> renderSourcePattern metadata b
  PrintfPattern fmt pat ->
    printf fmt <$> renderSourcePattern metadata pat

mappendMaybe :: Monoid m => Maybe m -> Maybe m -> Maybe m
mappendMaybe (Just a) (Just b) = Just (a <> b)
mappendMaybe _ _ = Nothing
