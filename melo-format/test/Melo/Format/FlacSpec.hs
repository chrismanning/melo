module Melo.Format.FlacSpec
  ( main,
    spec,
  )
where

import Data.ByteString.Base16 as Hex
import Data.Vector
import Melo.Format.Error
import Melo.Format.Flac
import Melo.Format.Info
import Melo.Format.Internal.Tag
import Melo.Format.Vorbis
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Flac" $ do
    it "parses flac" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      flac <- hReadFlac h
      flac `shouldSatisfy`
        (\case
           Flac _ -> True
           _ -> False)
      hClose h
    it "parses STREAMINFO" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      Flac flac <- hReadFlac h
      let (md5, _) = Hex.decode "ee67686246536453b5950b21810fde82"
      streamInfoBlock flac `shouldSatisfy`
        (\case
           StreamInfo { minBlockSize = 4096
                      , maxBlockSize = 4096
                      , minFrameSize = Just 11
                      , maxFrameSize = Just 13
                      , sampleRate = 44100
                      , channels = 1
                      , bps = 16
                      , samples = Just 44100
                      , md5 = md5
                      } -> True
           _ -> False)
    it "translates to Info" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      flac <- hReadFlac h
      let i = info flac
      sampleRate (i :: Info) `shouldBe` SampleRate 44100
      channels (i :: Info) `shouldBe` Mono
      bitsPerSample (i :: Info) `shouldBe` Just 16
      totalSamples (i :: Info) `shouldBe` Just 44100
    it "parses VORBIS COMMENT" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      Flac flac <- hReadFlac h
      vorbisComment flac `shouldBe` Just (VorbisComments "reference libFLAC 1.3.2 20170101" empty)
  describe "Flac with ID3v2" $
    it "reads flac file with ID3" $ do
      h <- openBinaryFile "test/Melo/silence-1s-id3v2.flac" ReadMode
      (FlacWithID3v2 id3 fs) <- hReadFlac h
      let Just v = vorbisComment fs
      readTags id3 `shouldBe`
        Tags
          (fromList
             [ ("TIT2", "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!")
             , ("TPE1", "κόσμε")
             , ("TALB", "イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン")
             , ("TRCK", "04")
             , ("TCON", "Psychedelic Rock")
             , ("TENC", "lame")
             , ("TLAN", "english")
             , ("WXXX;", "http://www.google.com")
             , ("TDRC", "2011")
             ])
      readTags v `shouldBe`
        Tags
          (fromList
             [ ("TITLE", "В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!")
             , ("ARTIST", "κόσμε")
             , ("ALBUM", "イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン")
             , ("GENRE", "Psychedelic Rock")
             , ("ENCODEDBY", "lame")
             , ("LANGUAGE", "english")
             , ("WWW", "http://www.google.com")
             , ("DATE", "2011")
             , ("TRACKNUMBER", "04")
             ])
      hClose h
  it "rejects non-flac files" $ do
    h <- openBinaryFile "test/Melo/silence-1s.ogg" ReadMode
    hReadFlac h `shouldThrow` (== UnknownFormat)
  it "rejects non-flac files with ID3v2" $ do
    h <- openBinaryFile "test/Melo/silence-1s-id3v24.mp3" ReadMode
    hReadFlac h `shouldThrow` (== UnknownFormat)
