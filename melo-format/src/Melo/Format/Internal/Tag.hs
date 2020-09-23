module Melo.Format.Internal.Tag where

import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Lens.Micro
import Melo.Format.Mapping

newtype Tags = Tags (Vector (Text, Text))
  deriving (Show, Eq, Generic)

class TagReader a where
  readTags :: a -> Tags

class TagWriter a where
  saveTags :: a -> Tags -> a

lookupTag :: Text -> Tags -> [Text]
lookupTag n (Tags ts) = V.toList $ fmap snd . V.filter ((== n) . fst) $ ts

getMappedTag :: FieldMappingSelector -> TagMapping -> Tags -> Vector Text
getMappedTag s (TagMapping ms) t =
  let ms' = fmap s ms
   in fromMaybe mempty $ V.find (not . null) (V.fromList $ NE.toList $ fmap (getTagByField t) ms')

getTagByField :: Tags -> FieldMapping -> Vector Text
getTagByField (Tags ts) m = fmap snd . V.filter (matches m . fst) $ ts
  where
    matches :: FieldMapping -> Text -> Bool
    matches NoFieldMapping _ = False
    matches fm v = fieldMatcher fm v

getMappedTagIndices :: FieldMappingSelector -> TagMapping -> Tags -> Vector Int
getMappedTagIndices s (TagMapping ms) t =
  let ms' = fmap s ms
   in V.concat $ NE.toList (fmap (getTagIndices t) ms')
  where
    getTagIndices :: Tags -> FieldMapping -> Vector Int
    getTagIndices (Tags tags) fm = let f = (fieldMatcher fm . fst) in V.findIndices f tags

setMappedTag :: (Functor f, Foldable f) => FieldMappingSelector -> TagMapping -> f Text -> Tags -> Tags
setMappedTag s tm@(TagMapping (fm :| _)) vs t@(Tags tags) =
  let is = getMappedTagIndices s tm t
      ft = V.ifilter (\i _ -> not (V.elem i is)) tags
   in Tags (ft <> V.fromList (toList (vs <&> (toCanonicalForm (s fm),))))

mappedTag :: FieldMappingSelector -> TagMapping -> TagLens
mappedTag s m f tags = (\vs -> setMappedTag s m vs tags) <$> f (getMappedTag s m tags)

type TagLens = Lens' Tags (Vector Text)
