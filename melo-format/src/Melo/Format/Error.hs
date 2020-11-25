module Melo.Format.Error where

import Control.Exception.Safe
import Data.Text
import Melo.Format.Internal.Metadata

data MetadataException
  = UnknownFormat
  | UnsupportedFormat
  | MetadataReadError !Text
  | MetadataWriteError !Text
  | MetadataNotFound !MetadataId
  | IncompatibleFormat !MetadataFileId !MetadataId
  deriving (Eq, Show)

instance Exception MetadataException
