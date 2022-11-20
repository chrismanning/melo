module Melo.Format.MP3Spec where

import Control.Exception.Safe
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import Lens.Micro
import Melo.Format.ID3.ID3v1
import Melo.Format.ID3.ID3v2
import qualified Melo.Format.Info as I
import Melo.Format.Internal.Metadata
import Melo.Format.MP3
import Melo.Format.Mapping as M
import Melo.Format.Metadata
import System.Directory
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
                  bitRate = FrameBitRate 64,
                  sampleRate = 44100,
                  padding = False,
                  private = False,
                  channels = Mono,
                  modeExtension = 0b00,
                  copyrighted = False,
                  original = True,
                  emphasis = 0b00,
                  crc = Nothing
                },
            id3v1 = Nothing,
            id3v2_3 = Nothing,
            id3v2_4 = Nothing,
            apev1 = Nothing,
            apev2 = Nothing,
            samples = Just 46080
          }
    it "reads mp3 file" $ do
      MetadataFile {..} <- readMp3File "test/Melo/silence-1s.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      metadata `shouldBe` H.empty
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 44100,
            channels = I.Mono,
            totalSamples = Just 46080,
            bitsPerSample = Nothing,
            quality = Just "64 kbps"
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
    it "writes mp3 file with id3v2.3" $ do
      let origFile = "test/Melo/silence-1s-id3v23.mp3"
      orig <- readMp3File origFile
      bracket (openBinaryTempFile "test/Melo/" "silence-1s-id3v23.mp3") (removeFile . fst) $ \(tmpfile, h') -> do
        hClose h'
        writeMp3File orig tmpfile
        writtenFileSize <- getFileSize tmpfile
        origFileSize <- getFileSize origFile
        !writtenContents <- withBinaryFile tmpfile ReadMode $ \h ->
          BS.hGet h (fromIntegral writtenFileSize)
        !origContents <- withBinaryFile origFile ReadMode $ \h ->
          BS.hGet h (fromIntegral origFileSize)
        writtenContents `shouldBe` origContents
    it "writes mp3 file with id3v2.4" $ do
      let origFile = "test/Melo/silence-1s-id3v24.mp3"
      orig <- readMp3File origFile
      bracket (openBinaryTempFile "test/Melo/" "silence-1s-id3v24.mp3") (removeFile . fst) $ \(tmpfile, h') -> do
        hClose h'
        writeMp3File orig tmpfile
        writtenFileSize <- getFileSize tmpfile
        origFileSize <- getFileSize origFile
        !writtenContents <- withBinaryFile tmpfile ReadMode $ \h ->
          BS.hGet h (fromIntegral writtenFileSize)
        !origContents <- withBinaryFile origFile ReadMode $ \h ->
          BS.hGet h (fromIntegral origFileSize)
        writtenContents `shouldBe` origContents
    it "reads mp3 file with id3v1" $ do
      MetadataFile {..} <- readMp3File "test/Melo/id3v1/id3v1_001_basic.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      let tag = mappedTag (fieldMappingSelector @ID3v1)
      metadata
        `shouldBe` H.singleton
          id3v1Id
          ( metadataFactory @ID3v1
              ( emptyTags
                  & tag M.trackTitle .~ V.singleton "Title"
                  & tag M.artist .~ V.singleton "Artist"
                  & tag M.album .~ V.singleton "Album"
                  & tag M.year .~ V.singleton "2003"
                  & tag M.commentTag .~ V.singleton "Comment"
                  & tag M.genre .~ V.singleton "Hip-Hop"
              )
          )
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 32000,
            channels = I.Mono,
            totalSamples = Just 2304,
            bitsPerSample = Nothing,
            quality = Just "64 kbps"
          }
    it "reads mp3 file with id3v2.3" $ do
      MetadataFile {..} <- readMp3File "test/Melo/silence-1s-id3v23.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      metadata
        `shouldBe` H.singleton
          id3v23Id
          ( metadataFactory @ID3v2_3
              ( Tags
                  ( V.fromList
                      [ ("TALB", "イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"),
                        ("TPE1", "κόσμε"),
                        ("TENC", "lame"),
                        ("TCON", "Psychedelic Rock"),
                        ("TLAN", "english"),
                        ("TIT2", "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"),
                        ("TRCK", "04"),
                        ("TYER", "2011"),
                        ("WXXX;", "http://google.com")
                      ]
                  )
              )
          )
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 44100,
            channels = I.Mono,
            totalSamples = Just 46080,
            bitsPerSample = Nothing,
            quality = Just "64 kbps"
          }
    it "reads mp3 file with id3v2.4" $ do
      MetadataFile {..} <- readMp3File "test/Melo/silence-1s-id3v24.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      metadata
        `shouldBe` H.singleton
          id3v24Id
          ( metadataFactory @ID3v2_4
              ( Tags
                  ( V.fromList
                      [ ("TALB", "イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"),
                        ("TPE1", "κόσμε"),
                        ("TENC", "lame"),
                        ("TCON", "Psychedelic Rock"),
                        ("TLAN", "english"),
                        ("TIT2", "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"),
                        ("TRCK", "04"),
                        ("TDRC", "2011"),
                        ("WXXX;", "http://google.com")
                      ]
                  )
              )
          )
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 44100,
            channels = I.Mono,
            totalSamples = Just 46080,
            bitsPerSample = Nothing,
            quality = Just "64 kbps"
          }
    it "reads mp3 file with id3v2.3 and extra padding" $ do
      MetadataFile {..} <- readMp3File "test/Melo/silence-1s-id3v23-extra-padding.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      metadata
        `shouldBe` H.singleton
          id3v23Id
          ( metadataFactory @ID3v2_3
              ( Tags
                  ( V.fromList
                      [ ("TALB", "イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"),
                        ("TPE1", "κόσμε"),
                        ("TENC", "lame"),
                        ("TCON", "Psychedelic Rock"),
                        ("TLAN", "english"),
                        ("TIT2", "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"),
                        ("TRCK", "04"),
                        ("TYER", "2011"),
                        ("WXXX;", "http://google.com")
                      ]
                  )
              )
          )
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 44100,
            channels = I.Mono,
            totalSamples = Just 46080,
            bitsPerSample = Nothing,
            quality = Just "64 kbps"
          }

    it "reads mp3 file with id3v2.4 and extra padding" $ do
      MetadataFile {..} <- readMp3File "test/Melo/silence-1s-id3v24-extra-padding.mp3"
      fileId `shouldBe` MetadataFileId "MP3"
      metadata
        `shouldBe` H.singleton
          id3v24Id
          ( metadataFactory @ID3v2_4
              ( Tags
                  ( V.fromList
                      [ ("TALB", "イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"),
                        ("TPE1", "κόσμε"),
                        ("TENC", "lame"),
                        ("TCON", "Psychedelic Rock"),
                        ("TLAN", "english"),
                        ("TIT2", "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"),
                        ("TRCK", "04"),
                        ("TDRC", "2011"),
                        ("WXXX;", "http://google.com")
                      ]
                  )
              )
          )
      audioInfo
        `shouldBe` I.Info
          { sampleRate = I.SampleRate 44100,
            channels = I.Mono,
            totalSamples = Just 46080,
            bitsPerSample = Nothing,
            quality = Just "64 kbps"
          }
