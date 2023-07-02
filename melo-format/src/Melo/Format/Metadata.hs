{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Melo.Format.Metadata
  ( SupportedMetadataFormats,
    Metadata (..),
    MetadataFile (..),
    MetadataId (..),
    MetadataFileFactory (..),
    MetadataFileId (..),
    MetadataFormatDesc (..),
    MetadataFormat (..),
    EmbeddedPicture (..),
    PictureType (..),
    openMetadataFile,
    openMetadataFileByExt,
    mkMetadata,
    metadataFactory,
    module Melo.Format.Internal.Tag,
    metadataFileFactoryIO,
    metadataFileFactoriesIO,
    fileFactoryByExt,
    tagLens,
  )
where

import Control.Exception.Safe
import Control.Monad
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.List (find, sortOn)
import Data.Ord
import Melo.Format.Ape
import Melo.Format.Error
import Melo.Format.Flac (Flac, flac)
import Melo.Format.ID3
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.MP3 (MP3, mp3)
import Melo.Format.OggVorbis (OggVorbis, oggVorbis)
import Melo.Format.Riff
import Melo.Format.Vorbis
import Melo.Format.Wav (wav)
import Melo.Format.WavPack (WavPack, wavPack)
import System.FilePath

metadataFileFactoriesIO :: [MetadataFileFactory IO]
metadataFileFactoriesIO = [flac, mp3, oggVorbis, wavPack, wav]

metadataFileFactoryIO :: MetadataFileId -> Maybe (MetadataFileFactory IO)
metadataFileFactoryIO mfid = find (\f -> f.fileId == mfid) metadataFileFactoriesIO

openMetadataFile :: FilePath -> IO MetadataFile
openMetadataFile p = do
  let fmts = sortOn (Down . (.priority)) metadataFileFactoriesIO
  fs <- filterM (\factory -> factory.detectFile p) fmts
  case fs of
    [] -> throwIO UnknownFormat
    (factory : _) -> factory.readMetadataFile p

openMetadataFileByExt :: FilePath -> IO MetadataFile
openMetadataFileByExt p =
  case takeExtension p of
    ".flac" -> flac.readMetadataFile p
    ".mp3" -> mp3.readMetadataFile p
    ".ogg" -> oggVorbis.readMetadataFile p
    ".wav" -> wav.readMetadataFile p
    ".wv" -> wavPack.readMetadataFile p
    ".wvpk" -> wavPack.readMetadataFile p
    _unknownExtension -> throwIO UnknownFormat

fileFactoryByExt :: FilePath -> Maybe (MetadataFileFactory IO)
fileFactoryByExt p =
  case takeExtension p of
    ".flac" -> Just flac
    ".mp3" -> Just mp3
    ".ogg" -> Just oggVorbis
    ".wav" -> Just wav
    ".wv" -> Just wavPack
    ".wvpk" -> Just wavPack
    _ -> Nothing

mkMetadata :: MetadataId -> Tags -> Maybe Metadata
mkMetadata = mk' @SupportedMetadataFormats

type SupportedMetadataFormats = '[VorbisComments, ID3v2_3, ID3v2_4, APEv2, RiffInfo, ID3v1, APEv1]

class MetadataFactory (a :: [Type]) where
  mk' :: MetadataId -> Tags -> Maybe Metadata

instance (MetadataFactory fs, MetadataFormat f) => MetadataFactory (f ': fs) where
  mk' mid tags = let m = metadataFormat @f in
    if m.formatId == mid
      then Just $ metadataFactory @f tags
      else mk' @fs mid tags

instance MetadataFactory '[] where
  mk' _ _ = Nothing
