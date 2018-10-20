module Melo.Internal.Tag where

import Data.Text

import Melo.Internal.Format

newtype Tags = Tags [(Text, Text)]
  deriving (Show, Eq)

class MetadataFormat a =>
      TagReader a
  where
  tags :: a -> Tags
