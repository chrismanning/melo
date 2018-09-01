module Melo.Format.OggVorbis where

import Data.ByteString
import qualified Data.ByteString.Lazy as L
import System.FilePath
import System.IO

import Melo.Format.Ogg
import Melo.Format.Vorbis
import Melo.Mapping as M(FieldMappings(vorbis))

import Melo.Internal.Binary
import Melo.Internal.Detect
import Melo.Internal.Format

data OggVorbis = OggVorbis Identification FramedVorbisComments
  deriving (Eq, Show)

instance BinaryGet OggVorbis where
  bget = do
    OggPage (IdentificationHeader ident) <- bget
    OggPage (CommentsHeader vc) <- bget
    return $ OggVorbis ident vc

instance MetadataFormat OggVorbis where
  formatDesc = "OggVorbis"

instance MetadataLocator OggVorbis

instance MetadataReader OggVorbis where
  tags (OggVorbis _ (FramedVorbisComments vc)) = getVorbisTags vc

instance Detector OggVorbis where
  fileDetectFormat p = if takeExtension p == ".ogg" then
    Just $ Detected readOggVorbisFile M.vorbis
    else Nothing
  hDetectFormat h = do
    hSeek h AbsoluteSeek 0
    buf <- hGet h 4
    if buf == "fLaC" then
      return $ Just $ Detected hReadOggVorbis M.vorbis
    else
      return Nothing

readOggVorbisFile :: FilePath -> IO OggVorbis
readOggVorbisFile p = bdecode <$> L.readFile p

hReadOggVorbis :: Handle -> IO OggVorbis
hReadOggVorbis h = bdecode <$> L.hGetContents h
