{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Detect(
  Detector(..)
, Detected(..)
, DetectedP(..)
, mkDetected
, getTag
) where

import Data.Text
import System.IO

import Melo.Format
import Melo.Internal.Info
import Melo.Internal.Tag
import Melo.Mapping

class Detector a where
  pathDetectFormat :: FilePath -> Maybe (DetectedP)
  hDetectFormat :: Handle -> IO (Maybe (DetectedP))

type MetadataReader a = (MetadataFormat a, InfoReader a, TagReader a)

data DetectedP where
  DetectedP :: MetadataReader a => Detected a -> DetectedP

data Detected a where
  Detected :: MetadataReader a =>
    { getHReadMetadata :: (Handle -> IO a)
    , getFieldSel :: FieldMappingSelector
    } -> Detected a

mkDetected :: MetadataReader a => (Handle -> IO a) -> FieldMappingSelector -> DetectedP
mkDetected rh s = DetectedP $ Detected rh s

getTag :: MetadataReader a => Detected a -> TagMapping -> a -> [Text]
getTag d m r = getMappedTag (getFieldSel d) m (tags r)
