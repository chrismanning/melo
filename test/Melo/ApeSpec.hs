module Melo.ApeSpec
  ( main
  , spec
  ) where

import Test.Hspec

import Data.Binary
import qualified Data.ByteString.Lazy as L

import Melo.Ape

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "APEv2" $ do
    it "parses APE tags" $ do
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

readApeTags :: FilePath -> IO APE
readApeTags p = decode <$> L.readFile p
