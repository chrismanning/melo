module Melo.Internal.Tag where

import           Data.Text

import           Melo.Internal.Format

newtype Tags = Tags [(Text, Text)]
  deriving (Show, Eq)

class MetadataFormat a =>
      TagReader a
  where
  tags :: a -> Tags

tagLookup :: Text -> Tags -> [Text]
tagLookup n (Tags ts) = linearLookup n ts
 where
  linearLookup _ [] = []
  linearLookup n ((k, v) : ts) | n == k    = v : linearLookup n ts
                               | otherwise = linearLookup n ts
