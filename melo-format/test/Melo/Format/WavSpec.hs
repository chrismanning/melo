module Melo.Format.WavSpec where

import Data.HashMap.Strict qualified as H
import Data.Vector qualified as V
import Lens.Micro
import Melo.Format.Info
import Melo.Format.Internal.Binary
import Melo.Format.Mapping
import Melo.Format.Metadata
import qualified Melo.Format.Wav as SUT
import qualified Melo.Format.Riff as SUT
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "WAVE files" $ do
    it "reads WAVE file" $ do
      wav <- SUT.readWavMetadataFile "test/Melo/silence-1s.wav"
      wav.fileId `shouldBe` SUT.wavFileId
      wav.audioInfo `shouldBe` Info {
        sampleRate = SampleRate 44100,
        channels = Mono,
        totalSamples = Just 44100,
        bitsPerSample = Just 16,
        quality = Nothing
      }
      wav.metadata `shouldBe` H.singleton SUT.riffId (metadataFactory @SUT.RiffInfo emptyTags)
    it "reads WAVE file with RIFF metadata" $ do
      wav <- SUT.readWavMetadataFile "test/Melo/silence-1s-riff.wav"
      wav.fileId `shouldBe` SUT.wavFileId
      wav.audioInfo `shouldBe` Info {
        sampleRate = SampleRate 44100,
        channels = Mono,
        totalSamples = Just 44100,
        bitsPerSample = Just 16,
        quality = Nothing
      }
      let metadata = metadataFactory @SUT.RiffInfo emptyTags & tagLens encoder .~ V.singleton "Lavf58.76.100"
      wav.metadata `shouldBe` H.singleton SUT.riffId metadata
