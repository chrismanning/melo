module Melo.Format.OggVorbis(
  OggVorbis(..)
, hReadOggVorbis
) where

import Data.ByteString
import System.FilePath
import System.IO

import Melo.Format.Ogg
import Melo.Format.Vorbis as V
import Melo.Mapping as M(FieldMappings(vorbis))

import Melo.Format
import Melo.Internal.Binary
import Melo.Internal.BinaryUtil
import Melo.Internal.Detect
import Melo.Internal.Info
import Melo.Internal.Locate
import Melo.Internal.Tag

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

instance TagReader OggVorbis where
  tags (OggVorbis _ (FramedVorbisComments vc)) = getVorbisTags vc

instance InfoReader OggVorbis where
  info (OggVorbis ident _) = Info {
    sampleRate = SampleRate $ fromIntegral $ V.sampleRate ident
  , channels = case V.channels ident of
      1 -> Mono
      2 -> Stereo
      _ -> MultiChannel ChannelMask
  , totalSamples = Nothing
  , bitsPerSample = Nothing
  }

instance Detector OggVorbis where
  pathDetectFormat p
    | takeExtension p == ".ogg" = Just detector
    | otherwise = Nothing
  hDetectFormat h = do
    hSeek h AbsoluteSeek 0
    buf <- hGet h 4
    -- FIXME ogg vorbis detection for handles
    if buf == "OggS" then
      return $ Just detector
    else
      return Nothing

detector :: DetectedP
detector = mkDetected hReadOggVorbis M.vorbis

hReadOggVorbis :: Handle -> IO OggVorbis
hReadOggVorbis h = do
  hSeek h AbsoluteSeek 0
  bdecode <$> hGetFileContents h
