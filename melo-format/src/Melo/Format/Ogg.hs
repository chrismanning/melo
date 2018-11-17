module Melo.Format.Ogg where

import           Data.Binary.Get
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L

import           Melo.Internal.Binary
import           Melo.Internal.BinaryUtil

newtype OggPage a = OggPage a

instance BinaryGet a => BinaryGet (OggPage a) where
  bget = do
    Page page <- bget
    return $ OggPage (runGet bget page)

newtype Page = Page L.ByteString

instance BinaryGet Page where
  bget = do
    expectGetEq (getByteString 4) "OggS" "Ogg capture pattern expected"
    expectGetEq getWord8 0 "Ogg stream structure version 0 expected"
    _typeFlags <- getWord8
    _position <- getInt64le
    _serialNum <- getWord32le
    _sequenceNum <- getWord32le
    _checksum <- getWord32le
    numSegments <- getWord8
    segmentTable <- getByteString $ fromIntegral numSegments
    let pageLength = sum (fmap fromIntegral (BS.unpack segmentTable))
    Page <$> getLazyByteString pageLength
