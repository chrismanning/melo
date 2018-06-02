module Melo.Format.WavPack where

import qualified Data.ByteString as BS
import System.IO

import Melo.Format.Ape
import Melo.Internal.Format

data WavPack =
  WavPack APE

ckId :: BS.ByteString
ckId = "wvpk"

readWavPack :: FilePath -> IO WavPack
readWavPack p = do
  h <- openBinaryFile p ReadMode
  WavPack <$> hGetMetadata h
