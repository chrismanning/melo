{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Service where

import Control.Lens hiding (from, lens)
import Data.Either
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Melo.Common.Logging
import qualified Melo.Database.Repo as Repo
import qualified Melo.Library.Source.Repo as Repo
import Melo.Library.Source.Types
import Network.URI
import System.Directory
import Witch

getAllSources :: Repo.SourceRepository m => m [Source]
getAllSources = rights <$> fmap tryFrom <$> Repo.getAll

getSource :: Repo.SourceRepository m => SourceRef -> m (Maybe Source)
getSource key = do
  srcs <- Repo.getByKey [key]
  pure $ listToMaybe $ rights $ tryFrom <$> srcs

importSources ::
  ( Repo.SourceRepository m,
    Logging m
  ) =>
  [NewImportSource] ->
  m [Source]
importSources [] = pure []
importSources ss = do
  $(logDebug) $ "Importing sources: " <> show ss
  let metadataSources :: [MetadataImportSource] = rights $ fmap tryFrom ss
  $(logDebug) $ "Importing metadata sources: " <> show metadataSources
  srcs <- Repo.insert (fmap from metadataSources)
  pure (rights $ fmap tryFrom srcs)

getSourcesByUriPrefix ::
  Repo.SourceRepository m =>
  URI ->
  m [Source]
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
  s <- listToMaybe <$> Repo.getByKey [key]
  case s >>= parseURI . T.unpack . (^. #source_uri) of
    Nothing -> pure Nothing
    Just uri ->
      case uriScheme uri of
        "file:" -> pure (Just $ unEscapeString $ uriPath uri)
        _otherScheme -> pure Nothing
