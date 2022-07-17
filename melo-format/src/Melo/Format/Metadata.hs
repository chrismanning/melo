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
    openMetadataFileByExt,
    mkMetadata,
    metadataFactory,
    module Melo.Format.Internal.Tag,
    metadataFileFactoryIO,
    metadataFileFactoriesIO,
  )
where

import Control.Exception.Safe
import Control.Monad
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List (find, sortOn)
import Data.Ord
import Lens.Micro
import Melo.Format.Ape
import Melo.Format.Error
import Melo.Format.Flac (Flac, flac)
import Melo.Format.ID3
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.MP3 (MP3, mp3)
import Melo.Format.OggVorbis (OggVorbis, oggVorbis)
import Melo.Format.Vorbis
import Melo.Format.WavPack (WavPack, wavPack)
import System.FilePath

type SupportedFormats = '[Flac, MP3, OggVorbis, WavPack]

metadataFileFactoriesIO :: [MetadataFileFactory IO]
metadataFileFactoriesIO = [flac, mp3, oggVorbis, wavPack]

metadataFileFactoryIO :: MetadataFileId -> Maybe (MetadataFileFactory IO)
metadataFileFactoryIO mfid = find (\f -> f ^. #fileId == mfid) metadataFileFactoriesIO

openMetadataFile :: FilePath -> IO MetadataFile
openMetadataFile p = do
  let fmts = sortOn (Down . (^. #priority)) metadataFileFactoriesIO
  fs <- filterM (\factory -> factory ^. #detectFile $ p) fmts
  case fs of
    [] -> throwIO UnknownFormat
    (factory : _) -> factory ^. #readMetadataFile $ p

openMetadataFileByExt :: FilePath -> IO MetadataFile
openMetadataFileByExt p =
  case takeExtension p of
    ".flac" -> flac ^. #readMetadataFile $ p
    ".mp3" -> mp3 ^. #readMetadataFile $ p
    ".ogg" -> oggVorbis ^. #readMetadataFile $ p
    ".wv" -> wavPack ^. #readMetadataFile $ p
    ".wvpk" -> wavPack ^. #readMetadataFile $ p
    _unknownExtension -> throwIO UnknownFormat

mkMetadata :: MetadataId -> Tags -> Maybe Metadata
mkMetadata = mk' @SupportedMetadataFormats

type SupportedMetadataFormats = '[APEv1, APEv2, ID3v1, ID3v2_3, ID3v2_4, VorbisComments]

class MetadataFactory (a :: [Type]) where
  mk' :: MetadataId -> Tags -> Maybe Metadata

instance (MetadataFactory fs, MetadataFormat f) => MetadataFactory (f ': fs) where
  mk' mid tags =
    if metadataFormat @f ^. #formatId == mid
      then Just $ metadataFactory @f tags
      else mk' @fs mid tags

instance MetadataFactory '[] where
  mk' _ _ = Nothing
