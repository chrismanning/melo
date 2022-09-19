module Melo.Format
  ( module Melo.Format.Ape,
    module Melo.Format.Flac,
    module Melo.Format.ID3,
    module Melo.Format.Info,
    module Melo.Format.Mapping,
    module Melo.Format.Metadata,
    module Melo.Format.OggVorbis,
    module Melo.Format.RIFF,
    module Melo.Format.Vorbis,
    module Melo.Format.WavPack,
    convert,
    convert',
  )
where

import Melo.Format.Ape
  ( APEv1 (..),
    APEv2 (..),
    apeTag,
    apeV1Id,
    apeV2Id,
  )
import Melo.Format.Flac
  ( Flac (),
    flac,
    removeID3,
  )
import Melo.Format.ID3
  ( ID3v1 (..),
    ID3v2 (..),
    ID3v2_3,
    ID3v2_4,
    id3v1Id,
    id3v1Tag,
    id3v23Id,
    id3v23Tag,
    id3v24Id,
    id3v24Tag,
  )
import Melo.Format.Info
import Melo.Format.Mapping
import Melo.Format.Metadata
import Melo.Format.OggVorbis
  ( OggVorbis (..),
    oggVorbis,
  )
import Melo.Format.Vorbis
  ( FramedVorbisComments (..),
    VorbisComments (..),
    vorbisCommentsId,
    vorbisTag,
  )
import Melo.Format.RIFF
  ( riffId,
  )
import Melo.Format.WavPack
  ( WavPack (..),
    wavPack,
  )
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector ((!?))
import Data.Functor ((<&>))

convert :: MetadataId -> Metadata -> Maybe Metadata
convert targetId m@Metadata{formatId} | targetId == formatId = Just m
convert targetId metadata = convert' targetId metadata mappings
  where
    mappings :: V.Vector TagMapping
    mappings = V.fromList defaultMappings

convert' :: MetadataId -> Metadata -> V.Vector TagMapping -> Maybe Metadata
convert' targetId m@Metadata{formatId} _ | targetId == formatId = Just m
convert' targetId Metadata{tags=(Tags tags), formatId} mappings = mkMetadata targetId emptyTags <&> \new -> new { tags = Tags (V.mapMaybe remap tags) }
  where
    remap :: (Text, Text) -> Maybe (Text, Text)
    remap (k, v) = do
      srcSelector <- selectorFor formatId
      targetSelector <- selectorFor targetId
      let findFieldMapping (TagMapping tm) = listToMaybe $ NE.filter (\fm -> fromMaybe False (fieldMatches <$> srcSelector fm <*> Just k)) tm
      fieldMapping <- V.mapMaybe findFieldMapping mappings !? 0
      fm <- canonicalForm <$> targetSelector fieldMapping
      Just (fm, v)
    selectorFor :: MetadataId -> Maybe FieldMappingSelector
    selectorFor mid | mid == vorbisCommentsId = Just vorbis
                    | mid == id3v24Id = Just id3v2_4
                    | mid == id3v23Id = Just id3v2_3
                    | mid == apeV2Id = Just ape
                    | mid == apeV1Id = Just ape
                    | mid == riffId = Just riff
                    | mid == id3v1Id = Just id3v1
                    | mid == MetadataId "CUE" = Just cue
                    | otherwise = Nothing
