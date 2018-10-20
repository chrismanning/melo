module Melo.Format.ApeSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.Binary
import qualified Data.ByteString.Lazy as L
import System.IO

import Melo.Format.Ape
import Melo.Internal.Locate

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "APEv2" $ do
    it "reads APEv2 tags" $ do
      readApeTags "test/Melo/test.apev2" `shouldReturn`
        APE
          APEv2
          [ mkTextTagItem "ALBUM" "Aqualung"
          , mkTextTagItem "ARTIST" "Jethro Tull"
          , mkTextTagItem "COMMENT" "CDP 32 1044 2"
          , mkTextTagItem "GENRE" "Progressive Rock"
          , mkTextTagItem "TITLE" "Cheap Day Return"
          , mkTextTagItem "TRACK" "3"
          , mkTextTagItem "YEAR" "1971"
          ]
    it "writes APEv2 tags" $ do
      let a =
            APE
              APEv2
              [ mkTextTagItem "ALBUM" "Aqualung"
              , mkTextTagItem "ARTIST" "Jethro Tull"
              , mkTextTagItem "COMMENT" "CDP 32 1044 2"
              , mkTextTagItem "GENRE" "Progressive Rock"
              , mkTextTagItem "TITLE" "Cheap Day Return"
              , mkTextTagItem "TRACK" "3"
              , mkTextTagItem "YEAR" "1971"
              ]
      let actual = encode a
      expected <- L.readFile "test/Melo/test.apev2"
      actual `shouldBe` expected
  describe "APEv1" $ do
    it "reads APEv1 tags" $ do
      readApeTags "test/Melo/test.apev1" `shouldReturn`
        APE
          APEv1
          [ mkTextTagItem "ALBUM" "Aqualung"
          , mkTextTagItem "ARTIST" "Jethro Tull"
          , mkTextTagItem "COMMENT" "CDP 32 1044 2"
          , mkTextTagItem "GENRE" "Progressive Rock"
          , mkTextTagItem "TITLE" "Cheap Day Return"
          , mkTextTagItem "TRACK" "3"
          , mkTextTagItem "YEAR" "1971"
          ]
    it "writes APEv1 tags" $ do
      let a =
            APE
              APEv1
              [ mkTextTagItem "ALBUM" "Aqualung"
              , mkTextTagItem "ARTIST" "Jethro Tull"
              , mkTextTagItem "COMMENT" "CDP 32 1044 2"
              , mkTextTagItem "GENRE" "Progressive Rock"
              , mkTextTagItem "TITLE" "Cheap Day Return"
              , mkTextTagItem "TRACK" "3"
              , mkTextTagItem "YEAR" "1971"
              ]
      let actual = encode a
      expected <- L.readFile "test/Melo/test.apev1"
      actual `shouldBe` expected
  describe "APE locator" $ do
    it "finds APEv2 from header" $ do
      bs <- L.readFile "test/Melo/test.apev2"
      locate @ APE bs `shouldBe` Just 0
    it "finds APE after padding" $ do
      bs <- L.readFile "test/Melo/test.apev2"
      locate @ APE (L.replicate 1000 1 `mappend` bs) `shouldBe` Just 1000
    it "finds APEv1 from footer" $ do
      bs <- L.readFile "test/Melo/test.apev1"
      locate @ APE bs `shouldBe` Just 0
    it "no APE found" $ do
      let bs = L.replicate 1000 0
      locate @ APE bs `shouldBe` Nothing
  describe "APE handle locator" $ do
    it "finds APEv2 from header" $ do
      h <- openBinaryFile "test/Melo/test.apev2" ReadMode
      hLocate @ APE h `shouldReturn` Just 0
    it "finds APEv1 from footer" $ do
      h <- openBinaryFile "test/Melo/test.apev1" ReadMode
      hLocate @ APE h `shouldReturn` Just 0
    it "no APE found" $ do
      h <- openBinaryFile "test/Melo/test.vorbiscomment" ReadMode
      hLocate @ APE h `shouldReturn` Nothing

readApeTags :: FilePath -> IO APE
readApeTags p = decode <$> L.readFile p
