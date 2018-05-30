module Melo.Format.VorbisSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.Binary
import qualified Data.ByteString.Lazy as L

import Melo.Format.Vorbis

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vorbis" $ do
    it "parses vendor string" $ do
      vc <- readVorbisComments "test/Melo/test.vorbiscomment"
      vc `shouldSatisfy`
        (\(VorbisComments vs _) -> vs == "reference libFLAC 1.2.1 20070917")
    it "parses user comments" $ do
      readVorbisComments "test/Melo/test.vorbiscomment" `shouldReturn`
        VorbisComments
          "reference libFLAC 1.2.1 20070917"
          [ UserComment "ALBUM" "Almost Heathen"
          , UserComment "GENRE" "Stoner Metal"
          , UserComment "DATE" "2001"
          , UserComment "TITLE" "Forty"
          , UserComment "TRACKNUMBER" "10"
          , UserComment "ARTIST" "Karma To Burn"
          ]

readVorbisComments :: FilePath -> IO VorbisComments
readVorbisComments p = decode <$> L.readFile p
