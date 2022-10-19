{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Service where

import Control.Lens hiding (from, lens)
import Data.Foldable
import qualified Data.Text as T
import Data.Time
import Data.Vector (Vector, empty, singleton)
import Melo.Common.Logging
import Melo.Common.Uri
import Melo.Common.Vector
import qualified Melo.Database.Repo as Repo
import qualified Melo.Library.Source.Repo as Repo
import Melo.Library.Source.Types
import System.Directory
import Witch

getAllSources :: Repo.SourceRepository m => m (Vector Source)
getAllSources = rights <$> fmap tryFrom <$> Repo.getAll

getSource :: Repo.SourceRepository m => SourceRef -> m (Maybe Source)
getSource key = do
  srcs <- Repo.getByKey (singleton key)
  pure $ firstOf traverse $ rights $ tryFrom <$> srcs

importSources ::
  ( Repo.SourceRepository m,
    Logging m
  ) =>
  Vector NewImportSource ->
  m (Vector Source)
importSources ss | null ss = pure empty
importSources ss = do
  $(logDebug) $ "Importing " <> show (length ss) <> " sources"
  let metadataSources = rights $ fmap tryFrom ss
  $(logDebug) $ "Importing " <> show (length metadataSources) <> " metadata sources"
  srcs <- Repo.insert (fmap (from @MetadataImportSource) metadataSources)
  pure (rights $ fmap tryFrom srcs)

getSourcesByUriPrefix ::
  Repo.SourceRepository m =>
  URI ->
  m (Vector Source)
getSourcesByUriPrefix prefix = do
  srcs <- Repo.getByUriPrefix prefix
  pure (rights $ fmap tryFrom srcs)

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

modificationTime :: NewImportSource -> IO LocalTime
modificationTime (FileSource _ f) = utcToLocalTime utc <$> getModificationTime (f ^. #filePath)
modificationTime (CueFileImportSource _ f) = utcToLocalTime utc <$> getModificationTime (f ^. #cueFilePath)

getSourceFilePath :: (Repo.SourceRepository m) => SourceRef -> m (Maybe FilePath)
getSourceFilePath key = do
  s <- firstOf traverse <$> Repo.getByKey (singleton key)
  pure (s >>= parseURI . T.unpack . (^. #source_uri) >>= uriToFilePath)
