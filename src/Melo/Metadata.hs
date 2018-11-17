module Melo.Metadata
  ( Metadata(..)
  , MetadataException(..)
  )
where

import           Control.Exception

import           Melo.Mapping

data Metadata = Tag TagMapping | AudioProperty

data MetadataException = UnknownFormat
  deriving Show

instance Exception MetadataException
