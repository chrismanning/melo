module Melo.Format.Internal.Tag where

import           Data.Text
import           GHC.Generics

import           Melo.Format.Internal.Format

newtype Tags = Tags [(Text, Text)]
  deriving (Show, Eq, Generic)

class MetadataFormat a => TagReader a
  where
  tags :: a -> Tags

class MetadataFormat a => TagWriter a where
  saveTags :: a -> Tags -> a

lookupTag :: Text -> Tags -> [Text]
lookupTag n (Tags ts) = linearLookup n ts
 where
  linearLookup _ [] = []
  linearLookup n' ((k, v) : ts') | n' == k   = v : linearLookup n' ts'
                                 | otherwise = linearLookup n' ts'
