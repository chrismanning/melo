module Melo.Format.OggVorbis
  ( OggVorbis(..)
  , hReadOggVorbis
  )
where

import           Data.Binary.Get
import           System.IO

import           Melo.Format.Ogg
import           Melo.Format.Vorbis            as V
import           Melo.Format.Mapping           as M
                                                          ( FieldMappings
                                                            ( vorbis
                                                            )
                                                          )

import           Melo.Format.Format
import           Melo.Format.Internal.Binary
import           Melo.Format.Internal.BinaryUtil
import           Melo.Format.Internal.Detect
import           Melo.Format.Internal.Info
import           Melo.Format.Internal.Locate
import           Melo.Format.Internal.Tag

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
  , totalSamples = Nothing -- TODO ogg vorbis total samples
  , bitsPerSample = Nothing
  }

instance Detector OggVorbis where
  -- file extension is not enough to identify vorbis inside ogg
  pathDetectFormat _ = Nothing
  hDetectFormat h = do
    hSeek h AbsoluteSeek 0
    buf <- hGetFileContents h
    return $ case runGetOrFail bget buf of
      Right (_, _, !(_ :: OggPage Header)) -> Just detector
      Left _ -> Nothing

detector :: DetectedP
detector = mkDetected hReadOggVorbis M.vorbis

hReadOggVorbis :: Handle -> IO OggVorbis
hReadOggVorbis h = do
  hSeek h AbsoluteSeek 0
  bdecodeOrThrowIO =<< hGetFileContents h
