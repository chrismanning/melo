{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Service where

import Control.Algebra
import Control.Applicative ((<|>))
import Control.Carrier.Error.Church
import Control.Carrier.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Effect.Lift
import Control.Effect.TH
import Control.Lens
import Control.Monad
import Data.Either.Combinators
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Pool
import qualified Data.Text as T
import Data.Text.Lens
import Database.PostgreSQL.Simple (Connection)
import Melo.Common.Effect
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import qualified Melo.Database.Model as DB
import Melo.Database.Transaction
import qualified Melo.Format as F
import Melo.Format.Error
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo as Repo
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import qualified System.FSNotify as FS
import System.FilePath
import Text.Printf

data CollectionService :: Effect where
  AddCollection :: NewCollection -> CollectionService m CollectionRef
  DeleteCollection :: CollectionRef -> CollectionService m ()
  RescanCollection :: CollectionRef -> CollectionService m ()

makeSmartConstructors ''CollectionService

newtype CollectionServiceIOC m a = CollectionServiceIOC
  { runCollectionServiceIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

runCollectionServiceIO :: CollectionServiceIOC m a -> m a
runCollectionServiceIO = runCollectionServiceIOC

instance
  ( Has CollectionRepository sig m,
    Has FileSystemService sig m,
    Has FileSystemWatchService sig m,
    Has Transaction sig m,
    Has (Lift IO) sig m,
    Has (Reader (Pool Connection)) sig m,
    Has (Reader (TVar (H.HashMap CollectionRef FS.StopListening))) sig m,
    Has Logging sig m
  ) =>
  Algebra (CollectionService :+: sig) (CollectionServiceIOC m)
  where
  alg hdl sig ctx = case sig of
    L (AddCollection c@NewFilesystemCollection {..}) -> do
      $(logInfo) $ "Adding collection " <> name
      $(logDebug) $ "Adding collection " <> show c
      cs <- Repo.insertCollections [c]
      pool <- ask @(Pool Connection)
      collectionWatchState <- ask @(TVar (H.HashMap CollectionRef FS.StopListening))
      case cs of
        [DB.Collection {..}] -> do
          let ref = CollectionRef id
          _ <- sendIO $
            forkIO $
              runReader collectionWatchState $
                runStdoutLogging $
                  runFileSystemIO $
                    runReader pool $
                      runTransaction $
                        withTransaction $ \conn ->
                          runReader conn $
                            runFileSystemWatchServiceIO $
                              runError (\(e :: MetadataException) -> $(logError) $ "uncaught metadata exception: " <> show e) pure $
                                runSavepoint $
                                  runMetadataServiceIO $
                                    runSourceRepositoryIO $
                                      runFileSystemServiceIO $
                                        runCollectionRepositoryIO $
                                          runCollectionServiceIO $
                                            void $
                                              scanPath ref (T.unpack rootPath)
          when watch $ startWatching ref (T.unpack rootPath)
          pure $ ctx $> ref
        _otherwise -> error "unexpected insertCollections result"
    L (RescanCollection ref@(CollectionRef id)) -> do
      listToMaybe <$> getCollections [DB.CollectionKey id] >>= \case
        Just DB.Collection {id, root_uri} ->
          case parseURI (T.unpack root_uri) >>= uriToFilePath of
            Just rootPath -> do
              $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
              scanPath ref rootPath >> pure ()
            Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
        Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
      pure $ ctx $> ()
    L (DeleteCollection ref@(CollectionRef id)) -> do
      stopWatching ref
      deleteCollections [DB.CollectionKey id]
      pure $ ctx $> ()
    R other -> CollectionServiceIOC (alg (runCollectionServiceIOC . hdl) other ctx)

data SourceMoveError
  = FileSystemMoveError MoveError
  | PatternError
  | NoSuchSource
  | SourcePathError
  deriving (Show)

moveSourceWithPattern ::
  ( Has FileSystem sig m,
    Has SourceRepository sig m,
    Has CollectionRepository sig m,
    Has Logging sig m
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
  ( Has SourceRepository sig m,
    Has CollectionRepository sig m
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
  ( Has CollectionRepository sig m
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
  Literal p -> Just p
  OptionalMappingPattern mapping -> tags ^? lens mapping . _head . unpacked
  DefaultMappingPattern mapping pat ->
    renderSourcePattern metadata (OptionalMappingPattern mapping)
      <|> renderSourcePattern metadata pat
  PrintfPattern fmt pat ->
    printf fmt <$> renderSourcePattern metadata pat

data SourcePathPattern
  = Literal FilePath
  | OptionalMappingPattern F.TagMapping
  | DefaultMappingPattern F.TagMapping SourcePathPattern
  | PrintfPattern String SourcePathPattern
