module Melo.Format.Internal.Tag where

import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Lens.Micro
import Melo.Format.Mapping

newtype Tags = Tags (Vector (Text, Text))
  deriving (Show, Eq, Generic)

emptyTags :: Tags
emptyTags = Tags V.empty

lookupTag :: Text -> Tags -> [Text]
lookupTag n (Tags ts) = V.toList $ fmap snd . V.filter ((== n) . fst) $ ts

getMappedTag :: FieldMappingSelector -> TagMapping -> Tags -> Vector Text
getMappedTag s (TagMapping ms) t =
  let ms' = catMaybes $ NE.toList $ fmap s ms
   in fromMaybe mempty $ V.find (not . null) (V.fromList $ fmap (getTagByField t) ms')

getMappedTag' :: FieldMappingSelector -> TagMapping -> Tags -> Maybe (FieldMapping, Vector Text)
getMappedTag' s (TagMapping ms) t =
  let ms' = catMaybes $ NE.toList $ fmap s ms
   in find (\(_m, vs) -> not $ V.null vs) $ fmap (\m -> (m, getTagByField t m)) ms'

getTagByField :: Tags -> FieldMapping -> Vector Text
getTagByField (Tags ts) m = V.filter (not . T.null) $ fmap snd . V.filter (fieldMatches m . fst) $ ts

getMappedTagIndices :: FieldMappingSelector -> TagMapping -> Tags -> Vector Int
getMappedTagIndices s (TagMapping ms) t =
  let ms' = fmap s ms
   in V.concat $ NE.toList (fmap (getTagIndices t) ms')
  where
    getTagIndices :: Tags -> Maybe FieldMapping -> Vector Int
    getTagIndices _ Nothing = V.empty
    getTagIndices (Tags tags) (Just fm) = let f = (fieldMatches fm . fst) in V.findIndices f tags

setMappedTag :: (Functor f, Foldable f) => FieldMappingSelector -> TagMapping -> f Text -> Tags -> Tags
setMappedTag s tm vs t | toList (getMappedTag s tm t) == toList vs = t
setMappedTag s tm@(TagMapping ms) vs t@(Tags tags) =
  let is = getMappedTagIndices s tm t
      ft = V.ifilter (\i _ -> not (V.elem i is)) tags
      fm =
        ms
          ^.. each
            . filtered
              ( \m -> case s m of
                  Nothing -> False
                  _ -> True
              )
   in case fm of
        (m : _) -> case s m of
           Just m' -> Tags (ft <> V.fromList (toList (vs <&> (canonicalForm m',))))
           Nothing -> t
        [] -> t

mappedTag :: FieldMappingSelector -> TagMapping -> TagLens
mappedTag s m f tags = (\vs -> setMappedTag s m vs tags) <$> f (getMappedTag s m tags)

type TagLens = Lens' Tags (Vector Text)

defaultMappings :: [TagMapping]
defaultMappings =
  [ albumArtist,
    artist,
    album,
    trackTitle,
    trackNumber,
    year,
    genre,
    albumTitleTag,
    albumTitleSortTag,
    trackTitleTag,
    trackTitleSortTag,
    albumArtistTag,
    albumArtistSortTag,
    trackArtistTag,
    trackArtistSortTag,
    composerTag,
    performerTag,
    yearTag,
    trackNumberTag,
    totalTracksTag,
    trackTotalTag,
    genreTag,
    commentTag,
    discNumberTag,
    totalDiscsTag,
    discTotalTag,
    encodedBy,
    language
  ]
