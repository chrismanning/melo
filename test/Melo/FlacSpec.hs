module Melo.FlacSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.ByteString.Base16 as Hex

import Melo.Flac

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Flac" $ do
    it "parses flac" $ do
      flac <- readFlac "test/Melo/normal.flac"
      streamInfoBlock flac `shouldSatisfy`
        (\b ->
           case b of
             StreamInfo {} -> True
             _ -> False)
    it "parses STREAMINFO" $ do
      flac <- readFlac "test/Melo/normal.flac"
      let (md5, _) = Hex.decode "8b8ef26d3251925d283774b7e1f8f949"
      streamInfoBlock flac `shouldSatisfy`
        (\b ->
           case b of
             StreamInfo { minBlockSize = 4096
                        , maxBlockSize = 4096
                        , minFrameSize = Just 359
                        , maxFrameSize = Just 13272
                        , sampleRate = 44100
                        , channels = 2
                        , bps = 16
                        , samples = Just 9667896
                        , md5 = md5
                        } -> True
             _ -> False)
    it "fails" $ do
      flac <- readFlacOrFail "test/Melo/test.vorbiscomment"
      case flac of
        Left e -> do
          putStrLn $ "error: " ++ show e
        _ -> expectationFailure "expected failure"
