{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Melo.Format.Metadata
  ( SupportedFormats,
    Metadata (..),
    MetadataFile (..),
    MetadataId (..),
    MetadataFileFactory (..),
    MetadataFileId (..),
    openMetadataFile,
    mkMetadata,
    module Melo.Format.Internal.Tag,
  )
where

import Control.Applicative ((<|>))
import Control.Exception.Safe
import Control.Monad
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List
import Data.Ord
import Lens.Micro
import Melo.Format.Ape
import Melo.Format.Error
import Melo.Format.Flac (Flac, flac)
import Melo.Format.ID3
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.OggVorbis (OggVorbis, oggVorbis)
import Melo.Format.Vorbis
import Melo.Format.WavPack (WavPack, wavPack)

type SupportedFormats = '[Flac, WavPack, OggVorbis]

openMetadataFile :: FilePath -> IO MetadataFile
openMetadataFile p = do
  let fmts = sortOn (Down . (^. #priority)) [flac, oggVorbis, wavPack]
  fs <- filterM (\factory -> factory ^. #detectFile $ p) fmts
  case fs of
    [] -> throwIO UnknownFormat
    (factory : _) -> factory ^. #readMetadataFile $ p

mkMetadata :: MetadataId -> Tags -> Maybe Metadata
mkMetadata = mk' @SupportedMetadataFormats

type SupportedMetadataFormats = '[APEv1, APEv2, ID3v1, ID3v2_3, ID3v2_4, VorbisComments]

class MetadataFactory (a :: [Type]) where
  mk' :: MetadataId -> Tags -> Maybe Metadata

instance (MetadataFactory fs, MetadataFormat f) => MetadataFactory (f ': fs) where
  mk' mid tags = metadataFactory @f mid tags <|> mk' @fs mid tags

instance MetadataFactory '[] where
  mk' _ _ = Nothing
