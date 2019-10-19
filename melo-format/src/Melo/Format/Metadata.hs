module Melo.Format.Metadata
  ( Metadata (..),
    MetadataException (..),
  )
where

import Control.Exception
import Data.Text
import Melo.Format.Mapping

data Metadata = Tag TagMapping | AudioProperty

data MetadataException = UnknownFormat | UnsupportedFormat | MetadataReadError Text
  deriving (Eq, Show)

instance Exception MetadataException
