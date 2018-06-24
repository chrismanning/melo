module Melo.Format.WavPack where

import qualified Data.ByteString as BS
import System.IO

import Melo.Format.Ape
import Melo.Internal.Format
import Melo.Internal.Binary

data WavPack =
  WavPack APE

instance MetadataFormat WavPack where
  formatDesc = "WavPack"

instance MetadataReader WavPack where
  tags = undefined

instance BinaryGet WavPack where
  bget = undefined

ckId :: BS.ByteString
ckId = "wvpk"

readWavPack :: FilePath -> IO WavPack
readWavPack p = do
  h <- openBinaryFile p ReadMode
  WavPack <$> hGetMetadata h
