module Melo.Format.Wav where

import Codec.Audio.Wave
import Control.Exception.Safe
import Melo.Format.Internal.Info
import Melo.Format.Internal.Metadata

wavFileId :: MetadataFileId
wavFileId = MetadataFileId "WAVE"

wav :: MetadataFileFactory IO
wav =
  MetadataFileFactory
    { priority = 100,
      fileId = wavFileId,
      detectFile = \p -> catchAny (readWaveFile p *> pure True) (\_ -> pure False),
      readMetadataFile = readWavMetadataFile,
      writeMetadataFile = writeWavMetadataFile
    }

readWavMetadataFile :: FilePath -> IO MetadataFile
readWavMetadataFile filePath = do
  wav <- readWaveFile filePath

  pure MetadataFile {
    audioInfo = Info {
      sampleRate = SampleRate $ fromIntegral (waveSampleRate wav),
      totalSamples = Just $ fromIntegral (waveSamplesTotal wav),
      bitsPerSample = Just $ fromIntegral (waveBitsPerSample wav),
      channels = convertWavChannels wav,
      quality = Nothing
    },
    metadata = mempty,
    pictures = mempty,
    fileId = wavFileId,
    filePath
  }

convertWavChannels :: Wave -> Channels
convertWavChannels w | waveChannels w == 1 = Mono
convertWavChannels w | waveChannels w == 2 = Stereo
convertWavChannels _ = MultiChannel ChannelMask

writeWavMetadataFile :: MetadataFile -> FilePath -> IO ()
writeWavMetadataFile _ _ = error "hWriteWavMetadata unimplemented"
