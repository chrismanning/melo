{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Metadata where

import Control.Exception.Safe
import Control.Monad
import Data.Generics.Labels ()
import Lens.Micro
import Melo.Format.Error
import Melo.Format.Internal.Metadata
import Melo.Format.Flac (Flac, flac)
import Melo.Format.OggVorbis (OggVorbis, oggVorbis)
import Melo.Format.WavPack (WavPack, wavPack)

type SupportedFormats = '[Flac, WavPack, OggVorbis]

openMetadataFile :: FilePath -> IO MetadataFile
openMetadataFile p = do
  fs <- filterM (\factory -> factory ^. #detectFile $ p) [flac, oggVorbis, wavPack]
  case fs of
    [] -> throwIO UnknownFormat
    (factory:_) -> factory ^. #readMetadataFile $ p
