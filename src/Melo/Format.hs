module Melo.Format(
  Tags(..),
  getMappedTag,
  tags
) where

import Data.Foldable
import Data.Text (Text, toLower)

import Melo.Internal.Format
import Melo.Mapping

getMappedTag :: FieldMappingSelector -> TagMapping -> Tags -> [Text]
getMappedTag _ (TagMapping []) _ = []
getMappedTag s (TagMapping ms) t = let ms' = fmap s ms in
  concat $ find (not . null) (fmap (getTag t) ms')
  where
    getTag :: Tags -> FieldMapping -> [Text]
    getTag (Tags []) _ = []
    getTag (Tags ts) m = fmap snd . filter (matches m . fst) $ ts
    matches :: FieldMapping -> Text -> Bool
    matches (FieldMapping _ f) v = f v
    matches _ _ = False
