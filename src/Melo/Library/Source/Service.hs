{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Service where

import Basement.From
import Control.Algebra
import Control.Lens hiding (from, lens)
import Data.Foldable
import Data.Maybe
import Data.Time
import Melo.Common.Logging
import qualified Melo.Library.Source.Repo as Repo
import Melo.Library.Source.Types
import Network.URI
import System.Directory

getAllSources :: Has Repo.SourceRepository sig m => m [Source]
getAllSources = mapMaybe tryFrom <$> Repo.getAllSources

importSources ::
  ( Has Repo.SourceRepository sig m,
    Has Logging sig m
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
  ( Has Repo.SourceRepository sig m,
    Has Logging sig m
  ) =>
  URI ->
  m [Source]
getSourcesByUriPrefix p = error "unimplemented"

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

modificationTime :: NewImportSource -> IO LocalTime
modificationTime (FileSource _ f) = utcToLocalTime utc <$> getModificationTime (f ^. #filePath)
