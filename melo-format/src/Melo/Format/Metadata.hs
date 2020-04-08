module Melo.Format.Metadata where

import Control.Exception.Safe
import Control.Monad
import Data.Generics.Labels ()
import Data.List
import Lens.Micro
import Melo.Format.Error
import Melo.Format.Flac (Flac, flac)
import Melo.Format.Internal.Metadata
import Melo.Format.OggVorbis (OggVorbis, oggVorbis)
import Melo.Format.WavPack (WavPack, wavPack)
import Data.Ord

type SupportedFormats = '[Flac, WavPack, OggVorbis]

openMetadataFile :: FilePath -> IO MetadataFile
openMetadataFile p = do
  let fmts = sortOn (Down . (^. #priority)) [flac, oggVorbis, wavPack]
  fs <- filterM (\factory -> factory ^. #detectFile $ p) fmts
  case fs of
    [] -> throwIO UnknownFormat
    (factory:_) -> factory ^. #readMetadataFile $ p
