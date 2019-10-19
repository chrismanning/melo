module Melo.Format.OggVorbisSpec
  ( main,
    spec,
  )
where

import qualified Data.ByteString.Lazy as L
import Data.Vector
import Melo.Format.Internal.Binary
import Melo.Format.OggVorbis
import Melo.Format.Vorbis
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Ogg Vorbis" $ do
    it "read ogg vorbis" $ do
      OggVorbis ident (FramedVorbisComments vc) <-
        bdecode
          <$> L.readFile "test/Melo/silence-1s.ogg"
      ident `shouldBe` Identification
        { vorbisVersion = 0,
          channels = 1,
          sampleRate = 44100,
          bitrateMax = Nothing,
          bitrateNominal = Just 80000,
          bitrateMin = Nothing
        }
      vc
        `shouldBe` VorbisComments
          "Lavf57.83.100"
          (singleton (UserComment "encoder" "Lavc57.107.100 libvorbis"))
