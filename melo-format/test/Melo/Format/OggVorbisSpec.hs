module Melo.Format.OggVorbisSpec
  ( main,
    spec,
  )
where

import Control.Exception.Safe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Vector
import Melo.Format.Internal.Binary
import Melo.Format.Ogg
import Melo.Format.OggVorbis
import Melo.Format.Vorbis
import System.Directory
import System.IO
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Ogg Vorbis" $ do
    it "reads ogg vorbis file" $ do
      OggVorbis (OggPage _ ident _) (OggPage _ (FramedVorbisComments vc) _) <-
        withBinaryFile "test/Melo/silence-1s.ogg" ReadMode hReadOggVorbis
      ident
        `shouldBe` Identification
          { vorbisVersion = 0,
            channels = 1,
            sampleRate = 44100,
            bitrateMax = Nothing,
            bitrateNominal = Just 80000,
            bitrateMin = Nothing,
            blockSize = 184
          }
      vc
        `shouldBe` VorbisComments
          "Lavf57.83.100"
          (singleton (UserComment "encoder" "Lavc57.107.100 libvorbis"))
    it "writes ogg vorbis file" $ do
      let origFile = "test/Melo/silence-1s.ogg"
      orig <- readOggVorbisFile origFile
      bracket (openBinaryTempFile "test/Melo/" "silence-1s.ogg") (removeFile . fst) $ \(tmpfile, h') -> do
        hClose h'
        writeOggVorbisFile orig tmpfile
        writtenFileSize <- getFileSize tmpfile
        origFileSize <- getFileSize origFile
        !writtenContents <- withBinaryFile tmpfile ReadMode $ \h ->
          BS.hGet h (fromIntegral writtenFileSize)
        !origContents <- withBinaryFile origFile ReadMode $ \h ->
          BS.hGet h (fromIntegral origFileSize)
        writtenContents `shouldBe` origContents
