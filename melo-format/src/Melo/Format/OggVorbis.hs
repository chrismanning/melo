module Melo.Format.OggVorbis
  ( OggVorbis (..),
    hReadOggVorbis,
    oggVorbisFileId,
    oggVorbis,
  )
where

import Data.Binary.Get
import qualified Data.HashMap.Strict as H
import Lens.Micro
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Info
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Ogg
import Melo.Format.Vorbis as V
import System.IO

oggVorbisFileId :: MetadataFileId
oggVorbisFileId = MetadataFileId "OggVorbis"

oggVorbis :: MetadataFileFactory IO
oggVorbis =
  MetadataFileFactory
    { priority = 100,
      fileId = oggVorbisFileId,
      readMetadataFile = \p -> do
        ogg <- withBinaryFile p ReadMode hReadOggVorbis
        pure
          MetadataFile
            { audioInfo = info ogg,
              fileId = oggVorbisFileId,
              metadata = oggVorbisMetadata ogg,
              filePath = p
            },
      detectFile = \p -> withBinaryFile p ReadMode $ \h -> do
        hSeek h AbsoluteSeek 0
        buf <- hGetFileContents h
        pure $ case runGetOrFail bget buf of
          Right (_, _, !(_ :: OggPage Header)) -> True
          Left _ -> False
    }

oggVorbisMetadata :: OggVorbis -> H.HashMap MetadataId Metadata
oggVorbisMetadata (OggVorbis _ (FramedVorbisComments vc)) =
  let fmt = metadataFormat @VorbisComments
   in H.singleton (fmt ^. #formatId) (extractMetadata vc)

data OggVorbis = OggVorbis !Identification !FramedVorbisComments
  deriving (Eq, Show)

instance BinaryGet OggVorbis where
  bget = do
    OggPage (IdentificationHeader ident) <- bget
    OggPage (CommentsHeader vc) <- bget
    return $ OggVorbis ident vc

instance MetadataLocator OggVorbis

--instance TagReader OggVorbis where
--  tags (OggVorbis _ (FramedVorbisComments vc)) = getVorbisTags vc

instance InfoReader OggVorbis where
  info (OggVorbis ident _) =
    Info
      { sampleRate = SampleRate $ fromIntegral $ V.sampleRate ident,
        channels = case V.channels ident of
          1 -> Mono
          2 -> Stereo
          _ -> MultiChannel ChannelMask,
        totalSamples = Nothing, -- TODO ogg vorbis total samples
        bitsPerSample = Nothing,
        quality = Nothing -- TODO ogg vorbis quality
      }

hReadOggVorbis :: Handle -> IO OggVorbis
hReadOggVorbis h = do
  hSeek h AbsoluteSeek 0
  bdecodeOrThrowIO =<< hGetFileContents h
