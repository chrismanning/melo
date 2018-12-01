module Melo.Format.Metadata
  ( Metadata(..)
  , MetadataException(..)
  )
where

import           Control.Exception

import           Melo.Format.Mapping

data Metadata = Tag TagMapping | AudioProperty

data MetadataException = UnknownFormat
  deriving Show

instance Exception MetadataException
