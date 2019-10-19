module Melo.Format.ID3.ID3v1Spec
  ( main,
    spec,
  )
where

import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Text ()
import Melo.Format.Format
import Melo.Format.ID3.ID3v1
import Melo.Format.Internal.Binary
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Tag
import qualified Melo.Format.Mapping as M
import Melo.Format.TestUtil
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ID3v1" $ do
    context "with valid input" $ do
      it "finds ID3v1" $ do
        h <- openBinaryFile "test/Melo/id3v1/id3v1_001_basic.mp3" ReadMode
        id3pos <- hLocate @ID3v1 h
        id3pos `shouldSatisfy` isJust
      it "parses basic ID3v1 #001" $
        readID3v1Tags "test/Melo/id3v1/id3v1_001_basic.mp3" `shouldReturn` ID3v1
          { title = "Title",
            artist = "Artist",
            album = "Album",
            year = "2003",
            comment = "Comment",
            track = Nothing,
            genre = "Hip-Hop"
          }
      it "parses basic ID3v1 #002" $
        readID3v1Tags "test/Melo/id3v1/id3v1_002_basic.mp3" `shouldReturn` ID3v1
          { title = "Title",
            artist = "Artist",
            album = "Album",
            year = "2003",
            comment = "Comment",
            track = Just 12,
            genre = "Hip-Hop"
          }
      it "parses basic ID3v1 #004" $
        readID3v1Tags "test/Melo/id3v1/id3v1_004_basic.mp3" `shouldReturn` ID3v1
          { title = "",
            artist = "",
            album = "",
            year = "2003",
            comment = "",
            track = Nothing,
            genre = "Blues"
          }
      it "parses basic ID3v1 #005" $
        readID3v1Tags "test/Melo/id3v1/id3v1_005_basic.mp3" `shouldReturn` ID3v1
          { title = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaA",
            artist = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbB",
            album = "cccccccccccccccccccccccccccccC",
            year = "2003",
            comment = "dddddddddddddddddddddddddddddD",
            track = Nothing,
            genre = "Blues"
          }
      it "parses basic ID3v1 #006" $
        readID3v1Tags "test/Melo/id3v1/id3v1_006_basic.mp3" `shouldReturn` ID3v1
          { title = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaA",
            artist = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbB",
            album = "cccccccccccccccccccccccccccccC",
            year = "2003",
            comment = "dddddddddddddddddddddddddddD",
            track = Just 1,
            genre = "Blues"
          }
      it "parses ID3v1 year #010" $
        readID3v1Tags "test/Melo/id3v1/id3v1_010_year.mp3" `shouldReturn` ID3v1
          { title = "",
            artist = "",
            album = "",
            year = "0000",
            comment = "",
            track = Nothing,
            genre = "Blues"
          }
      it "parses basic ID3v1 #011" $
        readID3v1Tags "test/Melo/id3v1/id3v1_011_year.mp3" `shouldReturn` ID3v1
          { title = "",
            artist = "",
            album = "",
            year = "9999",
            comment = "",
            track = Nothing,
            genre = "Blues"
          }
      it "parses non-latin characters" $
        readID3v1Tags "test/Melo/id3v1/id3v1_271_extra.mp3" `shouldReturn` ID3v1
          { title = "räksmörgås",
            artist = "räksmörgås",
            album = "räksmörgås",
            year = "2003",
            comment = "räksmörgås",
            track = Nothing,
            genre = "Blues"
          }
    context "with off-spec input" $ do
      it "parses additional genre" $
        readID3v1Tags "test/Melo/id3v1/id3v1_113_genre_W.mp3"
          `shouldReturn` ID3v1
            { title = "Easy Listening",
              artist = "",
              album = "",
              year = "2003",
              comment = "",
              track = Nothing,
              genre = "Easy Listening"
            }
      it "ignores junk" $
        readID3v1Tags "test/Melo/id3v1/id3v1_007_basic_W.mp3"
          `shouldReturn` ID3v1
            { title = "12345",
              artist = "12345",
              album = "12345",
              year = "2003",
              comment = "12345",
              track = Nothing,
              genre = "Blues"
            }
      it "ignores junk - id3v1.1" $
        readID3v1Tags "test/Melo/id3v1/id3v1_008_basic_W.mp3"
          `shouldReturn` ID3v1
            { title = "12345",
              artist = "12345",
              album = "12345",
              year = "2003",
              comment = "12345",
              track = Just 1,
              genre = "Blues"
            }
      it "parses space padded year" $
        readID3v1Tags "test/Melo/id3v1/id3v1_012_year_F.mp3"
          `shouldReturn` ID3v1
            { title = "",
              artist = "",
              album = "",
              year = "3",
              comment = "",
              track = Nothing,
              genre = "Blues"
            }
      it "parses short year" $
        readID3v1Tags "test/Melo/id3v1/id3v1_013_year_F.mp3"
          `shouldReturn` ID3v1
            { title = "",
              artist = "",
              album = "",
              year = "112",
              comment = "",
              track = Nothing,
              genre = "Blues"
            }
    context "with invalid input" $ do
      it "cannot find id3v1" $ do
        h <- openBinaryFile "test/Melo/id3v1/id3v1_003_basic_F.mp3" ReadMode
        hLocate @ID3v1 h `shouldReturn` Nothing
      it "fails to parse missing year" $ do
        id3v1 <- readID3v1Tags "test/Melo/id3v1/id3v1_014_year_F.mp3"
        evaluate id3v1 `shouldThrow` anyErrorCall
      it "fails to parse invalid genre" $ do
        id3v1 <- readID3v1Tags "test/Melo/id3v1/id3v1_270_genre_F.mp3"
        evaluate id3v1 `shouldThrow` anyErrorCall
    context "remove ID3v1" $ do
      it "does nothing to untagged file"
        $ withTempCopyOf "test/Melo/id3v1/no_tag.mp3"
        $ \h -> do
          x <- hLocate @ID3v1 h
          x `shouldSatisfy` isNothing
          hRemoveID3v1 h
          x' <- hLocate @ID3v1 h
          x' `shouldSatisfy` isNothing
      it "removes existing ID3v1 tag"
        $ withTempCopyOf "test/Melo/id3v1/id3v1_006_basic.mp3"
        $ \h -> do
          sz <- hFileSize h
          hRemoveID3v1 h
          sz' <- hFileSize h
          pure (sz, sz') `shouldReturn` (704, 576)
    context "replace ID3v1" $ do
      it "replaces existing ID3v1.1 tag"
        $ withTempCopyOf "test/Melo/id3v1/id3v1_006_basic.mp3"
        $ \h -> do
          oldTag <- hReadID3v1Tags h
          let tag = ID3v1
                { title = "title",
                  artist = "artist",
                  album = "album",
                  year = "2018",
                  comment = "comment",
                  track = Just 10,
                  genre = "Garage Rock"
                }
          oldTag `shouldNotBe` tag
          h `shouldReplaceWith` tag
      it "adds ID3v1 tag to tagless file"
        $ withTempCopyOf "test/Melo/id3v1/no_tag.mp3"
        $ \h -> do
          x <- hLocate @ID3v1 h
          x `shouldSatisfy` isNothing
          let tag = ID3v1
                { title = "title",
                  artist = "artist",
                  album = "album",
                  year = "2018",
                  comment = "comment",
                  track = Nothing,
                  genre = "Garage Rock"
                }
          h `shouldReplaceWith` tag
      it "adds ID3v1.1 tag to tagless file"
        $ withTempCopyOf "test/Melo/id3v1/no_tag.mp3"
        $ \h -> do
          x <- hLocate @ID3v1 h
          x `shouldSatisfy` isNothing
          let tag = ID3v1
                { title = "title",
                  artist = "artist",
                  album = "album",
                  year = "2018",
                  comment = "comment",
                  track = Just 10,
                  genre = "Garage Rock"
                }
          h `shouldReplaceWith` tag
    context "TagReader"
      $ it "maps ID3v1 fields to tags"
      $ do
        let tag = ID3v1
              { title = "title",
                artist = "artist",
                album = "album",
                year = "2018",
                comment = "comment",
                track = Just 10,
                genre = "Garage Rock"
              }
        let tags' = tags tag
        let getID3v1Tag = getMappedTag M.id3v1
        getID3v1Tag M.trackTitle tags' `shouldBe` ["title"]
        getID3v1Tag M.artist tags' `shouldBe` ["artist"]
        getID3v1Tag M.album tags' `shouldBe` ["album"]
        getID3v1Tag M.year tags' `shouldBe` ["2018"]
        getID3v1Tag M.commentTag tags' `shouldBe` ["comment"]
        getID3v1Tag M.trackNumber tags' `shouldBe` ["10"]
        getID3v1Tag M.genre tags' `shouldBe` ["Garage Rock"]

shouldReplaceWith :: Handle -> ID3v1 -> IO ()
shouldReplaceWith h tag = do
  hReplaceID3v1 h tag
  tag' <- hReadID3v1Tags h
  tag' `shouldBe` tag

readID3v1Tags :: FilePath -> IO ID3v1
readID3v1Tags p = do
  h <- openBinaryFile p ReadMode
  hReadID3v1Tags h

hReadID3v1Tags :: Handle -> IO ID3v1
hReadID3v1Tags h = do
  Just id3loc <- hLocate @ID3v1 h
  hSeek h AbsoluteSeek (fromIntegral id3loc)
  runGet bget <$> L.hGetContents h
