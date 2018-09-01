{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Detect where

import System.IO

import Melo.Internal.Format
import Melo.Mapping

class Detector a where
  fileDetectFormat :: FilePath -> Maybe (Detected FilePath)
  hDetectFormat :: Handle -> IO (Maybe (Detected Handle))

data Detected t where
  Detected :: MetadataReader a => (t -> IO a) -> FieldMappingSelector -> Detected t
