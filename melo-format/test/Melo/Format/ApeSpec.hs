module Melo.Format.ApeSpec
  ( main,
    spec,
  )
where

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Vector
import Melo.Format.Ape
import Melo.Format.Internal.Binary
import Melo.Format.Internal.Locate
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "APEv2" $ do
    it "reads APEv2 tags" $
      readApeV2Tags "test/Melo/test.apev2"
        `shouldReturn` ( APEv2
                           $ fromList
                             [ mkTextTagItem "ALBUM" "Aqualung",
                               mkTextTagItem "ARTIST" "Jethro Tull",
                               mkTextTagItem "COMMENT" "CDP 32 1044 2",
                               mkTextTagItem "GENRE" "Progressive Rock",
                               mkTextTagItem "TITLE" "Cheap Day Return",
                               mkTextTagItem "TRACK" "3",
                               mkTextTagItem "YEAR" "1971"
                             ]
                       )
    it "writes APEv2 tags" $ do
      let a =
            APEv2
              $ fromList
                [ mkTextTagItem "ALBUM" "Aqualung",
                  mkTextTagItem "ARTIST" "Jethro Tull",
                  mkTextTagItem "COMMENT" "CDP 32 1044 2",
                  mkTextTagItem "GENRE" "Progressive Rock",
                  mkTextTagItem "TITLE" "Cheap Day Return",
                  mkTextTagItem "TRACK" "3",
                  mkTextTagItem "YEAR" "1971"
                ]
      let actual = encode a
      expected <- L.readFile "test/Melo/test.apev2"
      actual `shouldBe` expected
  describe "APEv1" $ do
    it "reads APEv1 tags" $
      readApeV1Tags "test/Melo/test.apev1"
        `shouldReturn` ( APEv1
                           $ fromList
                             [ mkTextTagItem "ALBUM" "Aqualung",
                               mkTextTagItem "ARTIST" "Jethro Tull",
                               mkTextTagItem "COMMENT" "CDP 32 1044 2",
                               mkTextTagItem "GENRE" "Progressive Rock",
                               mkTextTagItem "TITLE" "Cheap Day Return",
                               mkTextTagItem "TRACK" "3",
                               mkTextTagItem "YEAR" "1971"
                             ]
                       )
    it "writes APEv1 tags" $ do
      let a =
            APEv1
              $ fromList
                [ mkTextTagItem "ALBUM" "Aqualung",
                  mkTextTagItem "ARTIST" "Jethro Tull",
                  mkTextTagItem "COMMENT" "CDP 32 1044 2",
                  mkTextTagItem "GENRE" "Progressive Rock",
                  mkTextTagItem "TITLE" "Cheap Day Return",
                  mkTextTagItem "TRACK" "3",
                  mkTextTagItem "YEAR" "1971"
                ]
      let actual = encode a
      expected <- L.readFile "test/Melo/test.apev1"
      actual `shouldBe` expected
  describe "APE locator" $ do
    it "finds APEv2 from header" $ do
      bs <- L.readFile "test/Melo/test.apev2"
      locate @APEv2 bs `shouldBe` Just 0
    it "finds APE after padding" $ do
      bs <- L.readFile "test/Melo/test.apev2"
      locate @APEv2 (L.replicate 1000 1 `mappend` bs) `shouldBe` Just 1000
    it "finds APEv1 from footer" $ do
      bs <- L.readFile "test/Melo/test.apev1"
      locate @APEv1 bs `shouldBe` Just 0
    it "no APE found" $ do
      let bs = L.replicate 1000 0
      locate @APEv1 bs `shouldBe` Nothing
  describe "APE handle locator" $ do
    it "finds APEv2 from header" $ do
      h <- openBinaryFile "test/Melo/test.apev2" ReadMode
      hLocate @APEv2 h `shouldReturn` Just 0
    it "finds APEv1 from footer" $ do
      h <- openBinaryFile "test/Melo/test.apev1" ReadMode
      hLocate @APEv1 h `shouldReturn` Just 0
    it "no APE found" $ do
      h <- openBinaryFile "test/Melo/test.vorbiscomment" ReadMode
      hLocate @APEv1 h `shouldReturn` Nothing

readApeV1Tags :: FilePath -> IO APEv1
readApeV1Tags p = bdecodeOrThrowIO =<< L.readFile p

readApeV2Tags :: FilePath -> IO APEv2
readApeV2Tags p = bdecodeOrThrowIO =<< L.readFile p
