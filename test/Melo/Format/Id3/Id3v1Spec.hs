module Melo.Format.Id3.Id3v1Spec
  ( main
  , spec
  ) where

import Test.Hspec

import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import System.IO

import Melo.Format.Id3.Id3v1
import Melo.Internal.Binary
import Melo.Internal.Format

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Id3v1" $ do
    context "with valid input" $ do
      it "finds Id3v1" $ do
        h <- openBinaryFile "test/Melo/id3v1/id3v1_001_basic.mp3" ReadMode
        id3pos <- hLocate @ID3v1 h
        id3pos `shouldSatisfy` isJust
      it "parses Id3v1" $ do
        readId3v1Tags "test/Melo/id3v1/id3v1_001_basic.mp3" `shouldReturn`
          ID3v1 {
            title = "Title"
            , artist = "Artist"
            , album = "Album"
            , year = "2003"
            , comment = "Comment"
            , track = Nothing
            , genre = "Hip-Hop"
          }
    context "with off-spec input" $ do
      it "parses additional genre" $ do
        readId3v1Tags "test/Melo/id3v1/id3v1_113_genre_W.mp3" `shouldReturn`
          ID3v1 {
            title = "Easy Listening"
            , artist = ""
            , album = ""
            , year = "2003"
            , comment = ""
            , track = Nothing
            , genre = "Easy Listening"
          }
      it "ignores junk" $ do
        readId3v1Tags "test/Melo/id3v1/id3v1_007_basic_W.mp3" `shouldReturn`
          ID3v1 {
            title = "12345"
            , artist = "12345"
            , album = "12345"
            , year = "2003"
            , comment = "12345"
            , track = Nothing
            , genre = "Blues"
          }
      it "ignores junk - id3v1.1" $ do
        readId3v1Tags "test/Melo/id3v1/id3v1_008_basic_W.mp3" `shouldReturn`
          ID3v1 {
            title = "12345"
            , artist = "12345"
            , album = "12345"
            , year = "2003"
            , comment = "12345"
            , track = Just 1
            , genre = "Blues"
          }
      it "parses space padded year" $ do
        readId3v1Tags "test/Melo/id3v1/id3v1_012_year_F.mp3" `shouldReturn`
          ID3v1 {
            title = ""
            , artist = ""
            , album = ""
            , year = "3"
            , comment = ""
            , track = Nothing
            , genre = "Blues"
          }
      it "parses short year" $ do
        readId3v1Tags "test/Melo/id3v1/id3v1_013_year_F.mp3" `shouldReturn`
          ID3v1 {
            title = ""
            , artist = ""
            , album = ""
            , year = "112"
            , comment = ""
            , track = Nothing
            , genre = "Blues"
          }
    context "with invalid input" $ do
      it "cannot find id3v1" $ do
        h <- openBinaryFile "test/Melo/id3v1/id3v1_003_basic_F.mp3" ReadMode
        hLocate @ID3v1 h `shouldReturn` Nothing
      it "fails to parse missing year" $ do
        id3v1 <- readId3v1Tags "test/Melo/id3v1/id3v1_014_year_F.mp3"
        evaluate id3v1 `shouldThrow` anyErrorCall
      it "fails to parse invalid genre" $ do
        id3v1 <- readId3v1Tags "test/Melo/id3v1/id3v1_270_genre_F.mp3"
        evaluate id3v1 `shouldThrow` anyErrorCall

readId3v1Tags :: FilePath -> IO ID3v1
readId3v1Tags p = do
  h <- openBinaryFile p ReadMode
  Just id3loc <- hLocate @ID3v1 h
  hSeek h AbsoluteSeek (fromIntegral id3loc)
  runGet bget <$> L.hGetContents h
