module Melo.Format.Wav where

import Codec.Audio.Wave
import Control.Exception.Safe
import Data.HashMap.Strict qualified as H
import Melo.Format.Error
import Melo.Format.Internal.Info
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Riff
import Streaming.Binary qualified as S
import Streaming.ByteString qualified as S

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

  riffTags <- case lookup "LIST" (waveOtherChunks wav) of
    Just chunk -> do
      (_, _, r) <- S.decodeWith (getRiffInfo) (S.fromStrict chunk)
      case r of
        Right (RiffInfo riffTags) -> pure riffTags
        Left _e -> pure emptyTags
    Nothing -> pure emptyTags

  -- TODO read id3v2 from wav files
  let metadata = H.singleton riffId $ metadataFactory @RiffInfo riffTags

  pure MetadataFile {
    audioInfo = Info {
      sampleRate = SampleRate $ fromIntegral (waveSampleRate wav),
      totalSamples = Just $ fromIntegral (waveSamplesTotal wav),
      bitsPerSample = Just $ fromIntegral (waveBitsPerSample wav),
      channels = convertWavChannels wav,
      quality = Nothing
    },
    metadata,
    pictures = mempty,
    fileId = wavFileId,
    filePath
  }

convertWavChannels :: Wave -> Channels
convertWavChannels w | waveChannels w == 1 = Mono
convertWavChannels w | waveChannels w == 2 = Stereo
convertWavChannels _ = MultiChannel ChannelMask

writeWavMetadataFile :: MetadataFile -> FilePath -> IO ()
-- TODO write WAV
writeWavMetadataFile _ _ = throwIO (MetadataWriteError "Cannot write WAV metadata")
