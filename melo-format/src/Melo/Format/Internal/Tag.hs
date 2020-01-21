module Melo.Format.Internal.Tag where

import Data.Foldable
import Data.Text (Text)
import GHC.Generics
import Lens.Micro
import Melo.Format.Mapping

newtype Tags = Tags [(Text, Text)]
  deriving (Show, Eq, Generic)

class TagReader a where
  readTags :: a -> Tags

class TagWriter a where
  saveTags :: a -> Tags -> a

lookupTag :: Text -> Tags -> [Text]
lookupTag n (Tags ts) = linearLookup n ts
  where
    linearLookup _ [] = []
    linearLookup n' ((k, v) : ts')
      | n' == k = v : linearLookup n' ts'
      | otherwise = linearLookup n' ts'

getMappedTag :: FieldMappingSelector -> TagMapping -> Tags -> [Text]
getMappedTag _ (TagMapping []) _ = []
getMappedTag s (TagMapping ms) t =
  let ms' = fmap s ms
   in concat $ find (not . null) (fmap (getTagByField t) ms')

getTagByField :: Tags -> FieldMapping -> [Text]
getTagByField (Tags []) _ = []
getTagByField (Tags ts) m = fmap snd . filter (matches m . fst) $ ts
  where
    matches :: FieldMapping -> Text -> Bool
    matches NoFieldMapping _ = False
    matches fm v = fieldMatcher fm v

setMappedTag :: FieldMappingSelector -> TagMapping -> [Text] -> Tags -> Tags
setMappedTag _ (TagMapping []) _ tags = tags
setMappedTag s (TagMapping ms) vs tags = undefined

mappedTag :: FieldMappingSelector -> TagMapping -> TagLens
mappedTag s m f tags = (\vs -> setMappedTag s m vs tags) <$> f (getMappedTag s m tags)

type TagLens = Lens' Tags [Text]

--type Age     = Int
--type City    = String
--type Country = String
--
--data Person = Person Age City Country
--
---- This lens lets you access all location-related information about a person.
--location :: Lens' Person (City, Country)
--location f (Person age city country) =
--  (\(city', country') -> Person age city' country') <$> f (city, country)
