module Melo.Format.WavPackSpec
  ( main,
    spec,
  )
where

import Data.Vector
import Melo.Format.Ape
import Melo.Format.ID3.ID3v1
import Melo.Format.WavPack
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "WavPack" $ do
    it "parses wavpack metadata header" $ do
      h <- openBinaryFile "test/Melo/silence-1s.wv" ReadMode
      wv <- hReadWavPack h
      wv `shouldSatisfy` \case
        WavPack (WavPackInfo {totalSamples = Just 44100, sampleSize = 16, channels = Mono, sampleRate = Just 44100, audioType = PCM}) _ ->
          True
        _ -> False
      hClose h
    it "parses ape in wavpack" $ do
      h <- openBinaryFile "test/Melo/silence-1s.wv" ReadMode
      wv <- hReadWavPack h
      let wvTags = wavPackTags wv
      wvTags `shouldBe` JustAPE (APEv2 (fromList [TextTagItem "encoder" (singleton "WavPack 5.1.0")]))
      hClose h
    it "parses id3v1 in wavpack" $ do
      h <- openBinaryFile "test/Melo/silence-1s-id3v1.wv" ReadMode
      wv <- hReadWavPack h
      wv `shouldSatisfy` \case
        WavPack _ (JustID3v1 (ID3v1 {title = "Title", artist = "Artist", album = "Album", year = "2003", comment = "Comment", track = Nothing, genre = "Hip-Hop"})) ->
          True
        _ -> False
      hClose h
    it "parses ape and id3v1 in wavpack" $ do
      h <- openBinaryFile "test/Melo/silence-1s-ape-id3v1.wv" ReadMode
      wv <- hReadWavPack h
      let wvTags = wavPackTags wv
      wvTags `shouldBe` Both (APEv2 (fromList [TextTagItem "encoder" (singleton "WavPack 5.1.0")])) (ID3v1 {title = "Title", artist = "Artist", album = "Album", year = "2003", comment = "Comment", track = Nothing, genre = "Hip-Hop"})
      hClose h
