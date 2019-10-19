module Melo.Format.VorbisSpec
  ( main,
    spec,
  )
where

import qualified Data.ByteString.Lazy as L
import Data.Vector
import Melo.Format.Internal.Binary
import Melo.Format.Vorbis
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vorbis Comments" $ do
    it "parses vendor string" $ do
      vc <- readVorbisComments "test/Melo/test.vorbiscomment"
      vc
        `shouldSatisfy` ( \(VorbisComments vs _) ->
                            vs == "reference libFLAC 1.2.1 20070917"
                        )
    it "parses user comments" $ do
      readVorbisComments "test/Melo/test.vorbiscomment"
        `shouldReturn` VorbisComments
          "reference libFLAC 1.2.1 20070917"
          -- FIXME rewrite tags
          ( fromList
              [ UserComment "ALBUM" "Almost Heathen",
                UserComment "GENRE" "Stoner Metal",
                UserComment "DATE" "2001",
                UserComment "TITLE" "Forty",
                UserComment "TRACKNUMBER" "10",
                UserComment "ARTIST" "Karma To Burn"
              ]
          )

readVorbisComments :: FilePath -> IO VorbisComments
readVorbisComments p = bdecode <$> L.readFile p
