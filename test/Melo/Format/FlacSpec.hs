module Melo.Format.FlacSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.ByteString.Base16 as Hex
import System.IO

import Melo.Format.Flac
import Melo.Format.Vorbis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Flac" $ do
    it "parses flac" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      flac <- hReadFlac h
      flac `shouldSatisfy`
        (\f ->
           case f of
             Flac _ -> True
             _ -> False)
      hClose h
    it "parses STREAMINFO" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      Flac flac <- hReadFlac h
      let (md5, _) = Hex.decode "ee67686246536453b5950b21810fde82"
      streamInfoBlock flac `shouldSatisfy`
        (\b ->
           case b of
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
    it "parses VORBIS COMMENT" $ do
      h <- openBinaryFile "test/Melo/silence-1s.flac" ReadMode
      Flac flac <- hReadFlac h
      vorbisComment flac `shouldBe` Just (VorbisComments "reference libFLAC 1.3.2 20170101" [])
    it "fails" $ do
      flac <- readFlacOrFail "test/Melo/test.vorbiscomment"
      case flac of
        Left e -> do
          putStrLn $ "error: " ++ show e
        _ -> expectationFailure "expected failure"
