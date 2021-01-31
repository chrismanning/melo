{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Service where

import Basement.From
import Control.Lens hiding (from, lens)
import Data.Foldable
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Melo.Common.Logging
import qualified Melo.Library.Source.Repo as Repo
import Melo.Library.Source.Types
import Network.URI
import System.Directory

getAllSources :: Repo.SourceRepository m => m [Source]
getAllSources = mapMaybe tryFrom <$> Repo.getAllSources

getSource :: Repo.SourceRepository m => SourceRef -> m (Maybe Source)
getSource ref = (>>= tryFrom) . listToMaybe <$> Repo.getSources [ref]

importSources ::
  ( Repo.SourceRepository m,
    Logging m
  ) =>
  [NewImportSource] ->
  m [Source]
importSources [] = pure []
importSources ss = do
  $(logDebug) $ "Importing sources: " <> show ss
  let metadataSources :: [MetadataImportSource] = mapMaybe tryFrom ss
  $(logDebug) $ "Importing metadata sources: " <> show metadataSources
  srcs <- Repo.insertSources (fmap from metadataSources)
  pure (mapMaybe tryFrom srcs)

getSourcesByUriPrefix ::
  Repo.SourceRepository m =>
  URI ->
  m [Source]
getSourcesByUriPrefix prefix = do
  srcs <- Repo.getSourcesByUriPrefix prefix
  pure (mapMaybe tryFrom srcs)

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

modificationTime :: NewImportSource -> IO LocalTime
modificationTime (FileSource _ f) = utcToLocalTime utc <$> getModificationTime (f ^. #filePath)

getSourceFilePath :: (Repo.SourceRepository m) => SourceRef -> m (Maybe FilePath)
getSourceFilePath k = do
  s <- listToMaybe <$> Repo.getSources [k]
  case s >>= parseURI . T.unpack . (^. #source_uri) of
    Nothing -> pure Nothing
    Just uri ->
      case uriScheme uri of
        "file:" -> pure (Just $ unEscapeString $ uriPath uri)
        _otherScheme -> pure Nothing
