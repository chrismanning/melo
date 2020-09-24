module Melo.Format.MP3Spec where

import Control.Exception.Safe
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as H
import qualified Melo.Format.Info as I
import Melo.Format.MP3
import Melo.Format.Metadata
import System.Directory
import System.FilePath
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "MP3" $ do
    it "parses mp3" $ do
      mp3 <- withBinaryFile "test/Melo/silence-1s.mp3" ReadMode $ \h ->
        hReadMp3 h
      mp3
        `shouldBe` MP3
          { frameHeader =
              FrameHeader
                { mpegAudioVersion = V1,
                  layer = Layer3,
                  bitRate = CBR 64,
                  sampleRate = 44100,
                  padding = False,
                  private = False,
                  channels = Mono,
                  modeExtension = 0b00,
                  copyrighted = False,
                  original = True,
                  emphasis = 0b00,
                  crc = Just 0
                },
            id3v1 = Nothing,
            id3v2_3 = Nothing,
            id3v2_4 = Nothing,
            apev1 = Nothing,
            apev2 = Nothing
          }
    it "reads mp3 file" $ do
      MetadataFile {..} <- readMp3File "test/Melo/silence-1s.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      metadata `shouldBe` H.empty
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 44100,
            channels = I.Mono,
            totalSamples = Nothing,
            bitsPerSample = Nothing,
            quality = Just "CBR 64"
          }
    it "writes mp3 file" $ do
      let origFile = "test/Melo/silence-1s.mp3"
      orig <- readMp3File origFile
      bracket (openBinaryTempFile "test/Melo/" "silence-1s.mp3") (removeFile . fst) $ \(tmpfile, h') -> do
        hClose h'
        writeMp3File orig tmpfile
        writtenFileSize <- getFileSize tmpfile
        origFileSize <- getFileSize origFile
        !writtenContents <- withBinaryFile tmpfile ReadMode $ \h ->
          BS.hGet h (fromIntegral writtenFileSize)
        !origContents <- withBinaryFile origFile ReadMode $ \h ->
          BS.hGet h (fromIntegral origFileSize)
        writtenContents `shouldSatisfy` (== origContents)
