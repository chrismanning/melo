module Melo.Format.ID3.ID3v2Spec
  ( main,
    spec,
  )
where

import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Melo.Format.ID3.ID3v2
import Melo.Format.Internal.Binary
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import qualified Melo.Format.Mapping as M
  ( FieldMappings
      ( id3v2_3,
        id3v2_4
      ),
  )
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ID3v2" $ do
    context "ID3v2.4"
      $ context "with valid input"
      $ do
        it "finds tags" $ do
          h <- openBinaryFile "test/Melo/vbr-id3v24.mp3" ReadMode
          id3pos <- hLocate @ID3v2 h
          id3pos `shouldSatisfy` isJust
        it "parses ID3v2.4" $ do
          id3 <- readID3v2Tags "test/Melo/vbr-id3v24.mp3"
          let t = readTags id3
          getMappedTag M.id3v2_4 artist t `shouldBe` ["κόσμε"]
          getMappedTag M.id3v2_4 trackTitle t
            `shouldBe` ["В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"]
          getMappedTag M.id3v2_4 album t
            `shouldBe` ["イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"]
          getMappedTag M.id3v2_4 genre t `shouldBe` ["Psychedelic Rock"]
    context "ID3v2.3" $ do
      context "with valid input" $ do
        it "finds tags" $ do
          h <- openBinaryFile "test/Melo/vbr-id3v23.mp3" ReadMode
          id3pos <- hLocate @ID3v2 h
          id3pos `shouldSatisfy` isJust
        it "parses ID3v2.3" $ do
          id3 <- readID3v2Tags "test/Melo/vbr-id3v23.mp3"
          let t = readTags id3
          getMappedTag M.id3v2_3 artist t `shouldBe` ["κόσμε"]
          getMappedTag M.id3v2_3 trackTitle t
            `shouldBe` ["В чащах юга жил бы цитрус? Да, но фальшивый экземпляр!"]
          getMappedTag M.id3v2_3 album t
            `shouldBe` ["イロハニホヘト チリヌルヲ ワカヨタレソ ツネナラム ウヰノオクヤマ ケフコエテ アサキユメミシ ヱヒモセスン"]
          getMappedTag M.id3v2_3 genre t `shouldBe` ["Psychedelic Rock"]
      context "with UTF-16 text"
        $ it "parses ID3v2.3"
        $ do
          id3 <- readID3v2Tags "test/Melo/silence-1s-id3v23.mp3"
          readTags id3
            `shouldBe` Tags
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

readID3v2Tags :: FilePath -> IO ID3v2
readID3v2Tags p = do
  h <- openBinaryFile p ReadMode
  Just id3loc <- hLocate @ID3v2 h
  hSeek h AbsoluteSeek (fromIntegral id3loc)
  runGet bget <$> L.hGetContents h
