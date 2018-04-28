module Melo.Flac where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Debug.Trace

readFlac :: FilePath -> IO Flac
readFlac p = do
  L.readFile p >>= return . decode

readFlacOrFail :: FilePath -> IO (Either (ByteOffset, String) Flac)
readFlacOrFail = decodeFileOrFail

newtype Flac = Flac
  { blocks :: [MetadataBlock]
  } deriving (Show)

data MetadataBlockHeader = MetadataBlockHeader
  { blockType :: Word8
  , blockLength :: Word32
  , isLast :: Bool
  } deriving (Show)

data MetadataBlock
  = StreamInfo { header :: MetadataBlockHeader
               , minBlockSize :: Word16
               , maxBlockSize :: Word16
               , minFrameSize :: Maybe Word32
               , maxFrameSize :: Maybe Word32
               , sampleRate :: Word32
               , channels :: Word8
               , bps :: Word8
               , samples :: Word64
               , md5 :: L.ByteString }
  | Padding { header :: MetadataBlockHeader }
  | Application { header :: MetadataBlockHeader }
  | SeekTable { header :: MetadataBlockHeader }
  | VorbisComment { header :: MetadataBlockHeader }
  | CueSheet { header :: MetadataBlockHeader }
  | Picture { header :: MetadataBlockHeader }
  deriving (Show)

instance Binary MetadataBlockHeader where
  put = undefined
  get = do
    byte <- getWord8
    let isLast = testBit byte 7
    let blockType = clearBit byte 7
    len <- get24Bits
    return $ MetadataBlockHeader blockType len isLast

instance Binary Flac where
  put = undefined
  get = do
    marker <- getByteString 4
    guard (marker == "fLaC")
    Flac <$> getMetadataBlocks

getMetadataBlocks :: Get [MetadataBlock]
getMetadataBlocks = go
  where
    go = do
      header <- get :: Get MetadataBlockHeader
      traceM $ show header
      block <- getMetadataBlock header
      traceM $ show block
      if isLast header
        then return [block]
        else do
          blocks <- go
          return $ block : blocks

getMetadataBlock :: MetadataBlockHeader -> Get MetadataBlock
getMetadataBlock header =
  case blockType header of
    0 -> getStreamInfoBlock header
    _ -> getPadding header

getStreamInfoBlock :: MetadataBlockHeader -> Get MetadataBlock
getStreamInfoBlock header = do
  minBlockSize <- getWord16be
  maxBlockSize <- getWord16be
  minFrameSize <- get24Bits
  maxFrameSize <- get24Bits
  rest <- getWord64be
  let sampleRate = fromIntegral $ ((shiftR rest 44) .&. 0xFFFFF)
  let channels = fromIntegral $ ((shiftR rest 41) .&. 0b111) + 1
  let bps = fromIntegral $ ((shiftR rest 36) .&. 0b11111) + 1
  let samples = fromIntegral $ (rest .&. 0xFFFFFFFFF)
  md5 <- getLazyByteString 16
  return $
    StreamInfo
      { header
      , minBlockSize
      , maxBlockSize
      , minFrameSize = mfilter (> 0) $ Just minFrameSize
      , maxFrameSize = mfilter (> 0) $ Just maxFrameSize
      , sampleRate
      , channels
      , bps
      , samples
      , md5
      }

getPadding :: MetadataBlockHeader -> Get MetadataBlock
getPadding header = do
  traceM $ "found padding; skipping " ++ (show $ blockLength header)
  skip $ fromIntegral $ blockLength header
  return $ Padding header

get24Bits :: (Num a, Bits a) => Get a
get24Bits = do
  x <- liftM fromIntegral getWord8
  y <- liftM fromIntegral getWord8
  z <- liftM fromIntegral getWord8
  return $ (shiftL x 16) .|. (shiftL y 8) .|. z
