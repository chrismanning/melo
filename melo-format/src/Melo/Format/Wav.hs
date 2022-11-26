module Melo.Format.Wav where

import Codec.Audio.Wave
import Control.Applicative
import Control.Exception.Safe
import Data.HashMap.Strict qualified as H
import Data.Maybe
import Melo.Format.Error
import Melo.Format.Internal.Info
import Melo.Format.Internal.Metadata
import Melo.Format.ID3
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

  let otherChunks = waveOtherChunks wav
  riff <- case lookup "LIST" otherChunks of
    Just chunk -> do
      (_, _, r) <- S.decodeWith getRiffInfo (S.fromStrict chunk)
      case r of
        Right (RiffInfo riffTags) -> pure $ Just $ metadataFactory @RiffInfo riffTags
        Left _e -> pure Nothing
    Nothing -> pure Nothing

  id3 <- case lookup "ID3 " otherChunks <|> lookup "id3 " otherChunks of
    Nothing -> pure Nothing
    Just chunk -> do
      (_, _, r) <- S.decode (S.fromStrict chunk)
      case r of
        Right id3v23 -> pure $ Just $ metadataFactory @ID3v2_3 (readTags @ID3v2_3 id3v23)
        Left _e -> do
          (_, _, r) <- S.decode (S.fromStrict chunk)
          case r of
            Right id3v24 -> pure $ Just $ metadataFactory @ID3v2_4 (readTags @ID3v2_4 id3v24)
            Left _e -> pure Nothing
  
  let metadata = H.fromList $ (\m -> (m.formatId, m)) <$> catMaybes [riff, id3]

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
