module Melo.Format.OggVorbis(
  OggVorbis(..)
, hReadOggVorbis
) where

import Data.ByteString
import qualified Data.ByteString.Lazy as L
import System.FilePath
import System.IO

import Melo.Format.Ogg
import Melo.Format.Vorbis
import Melo.Mapping as M(FieldMappings(vorbis))

import Melo.Format
import Melo.Internal.Binary
import Melo.Internal.Detect

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
  pathDetectFormat p
    | takeExtension p == ".ogg" = Just detector
    | otherwise = Nothing
  hDetectFormat h = do
    hSeek h AbsoluteSeek 0
    buf <- hGet h 4
    if buf == "fLaC" then
      return $ Just detector
    else
      return Nothing

detector :: DetectedP
detector = mkDetected hReadOggVorbis M.vorbis

hReadOggVorbis :: Handle -> IO OggVorbis
hReadOggVorbis h = bdecode <$> L.hGetContents h
