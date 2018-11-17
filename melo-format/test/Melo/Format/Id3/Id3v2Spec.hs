module Melo.Format.Id3.Id3v2Spec
  ( main
  , spec
  ) where

import Test.Hspec

import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import System.IO

import Melo.Format
import Melo.Format.Id3.Id3v2
import Melo.Internal.Binary
import Melo.Internal.Locate
import Melo.Internal.Tag
import Melo.Mapping
import qualified Melo.Mapping as M(FieldMappings(id3v2_4, id3v2_3))
import Melo.Tag

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Id3v2" $ do
    context "Id3v2.4" $ do
      context "with valid input" $ do
        it "finds tags" $ do
          h <- openBinaryFile "test/Melo/vbr-id3v24.mp3" ReadMode
          id3pos <- hLocate @Id3v2 h
          id3pos `shouldSatisfy` isJust
        it "parses Id3v2.4" $ do
          id3 <- readId3v2Tags "test/Melo/vbr-id3v24.mp3"
          let t = tags id3
          getMappedTag M.id3v2_4 artist t `shouldBe` ["κόσμε"]
          getMappedTag M.id3v2_4 trackTitle t `shouldBe` ["В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"]
          getMappedTag M.id3v2_4 album t `shouldBe` ["イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"]
          getMappedTag M.id3v2_4 genre t `shouldBe` ["Psychedelic Rock"]
    context "Id3v2.3" $ do
      context "with valid input" $ do
        it "finds tags" $ do
          h <- openBinaryFile "test/Melo/vbr-id3v23.mp3" ReadMode
          id3pos <- hLocate @Id3v2 h
          id3pos `shouldSatisfy` isJust
        it "parses Id3v2.3" $ do
          id3 <- readId3v2Tags "test/Melo/vbr-id3v23.mp3"
          let t = tags id3
          getMappedTag M.id3v2_3 artist t `shouldBe` ["κόσμε"]
          getMappedTag M.id3v2_3 trackTitle t `shouldBe` ["В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"]
          getMappedTag M.id3v2_3 album t `shouldBe` ["イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"]
          getMappedTag M.id3v2_3 genre t `shouldBe` ["Psychedelic Rock"]

readId3v2Tags :: FilePath -> IO Id3v2
readId3v2Tags p = do
  h <- openBinaryFile p ReadMode
  Just id3loc <- hLocate @Id3v2 h
  hSeek h AbsoluteSeek (fromIntegral id3loc)
  runGet bget <$> L.hGetContents h
