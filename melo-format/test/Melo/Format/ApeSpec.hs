module Melo.Format.ApeSpec
  ( main,
    spec,
  )
where

import Data.Binary
import qualified Data.ByteString.Lazy as L
import Data.Vector
import Melo.Format.Ape
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "APEv2" $ do
    it "reads APEv2 tags" $
      withBinaryFile "test/Melo/test.apev2" ReadMode hGetApe
        `shouldReturn` ( Just $ APEv2 $
                           fromList
                             [ mkTextTagItem "ALBUM" "Aqualung",
                               mkTextTagItem "ARTIST" "Jethro Tull",
                               mkTextTagItem "COMMENT" "CDP 32 1044 2",
                               mkTextTagItem "GENRE" "Progressive Rock",
                               mkTextTagItem "TITLE" "Cheap Day Return",
                               mkTextTagItem "TRACK" "3",
                               mkTextTagItem "YEAR" "1971"
                             ]
                       )
    it "cannot read non-existent APEv2 tags" $
      withBinaryFile "test/Melo/test.vorbiscomment" ReadMode (hGetApe @'V2) `shouldReturn` Nothing
    it "writes APEv2 tags" $ do
      let a =
            APEv2 $
              fromList
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
      withBinaryFile "test/Melo/test.apev1" ReadMode hGetApe
        `shouldReturn` ( Just $ APEv1 $
                           fromList
                             [ mkTextTagItem "ALBUM" "Aqualung",
                               mkTextTagItem "ARTIST" "Jethro Tull",
                               mkTextTagItem "COMMENT" "CDP 32 1044 2",
                               mkTextTagItem "GENRE" "Progressive Rock",
                               mkTextTagItem "TITLE" "Cheap Day Return",
                               mkTextTagItem "TRACK" "3",
                               mkTextTagItem "YEAR" "1971"
                             ]
                       )
    it "cannot read non-existent APEv1 tags" $
      withBinaryFile "test/Melo/test.vorbiscomment" ReadMode (hGetApe @'V1) `shouldReturn` Nothing
    it "writes APEv1 tags" $ do
      let a =
            APEv1 $
              fromList
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
