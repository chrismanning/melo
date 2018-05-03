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

data Flac = Flac
  { streamInfoBlock :: StreamInfo
  , metadataBlocks :: [MetadataBlock]
  } deriving (Show)

instance Binary Flac where
  put = undefined
  get = do
    marker <- getByteString 4
    guard (marker == "fLaC")
    Flac <$> get <*> getMetadataBlocks

getMetadataBlocks :: Get [MetadataBlock]
getMetadataBlocks = go
  where
    go = do
      header <- lookAhead get :: Get MetadataBlockHeader
      block <- get
      traceM $ show block
      if isLast header
        then return [block]
        else do
          blocks <- go
          return $ block : blocks

data MetadataBlock
  = StreamInfoBlock StreamInfo
  | PaddingBlock Padding
  | ApplicationBlock Application
  | SeekTableBlock SeekTable
  | VorbisCommentBlock
  | CueSheetBlock CueSheet
  | PictureBlock
  deriving (Show)

instance Binary MetadataBlock where
  put = undefined
  get = do
    header <- lookAhead get
    case blockType header of
      0 -> StreamInfoBlock <$> get
      1 -> PaddingBlock <$> get
      2 -> ApplicationBlock <$> get
      3 -> SeekTableBlock <$> get
      5 -> CueSheetBlock <$> get
      _ -> do
        header' <- get
        let len = blockLength header'
        skip $ fromIntegral $ len
        return $ PaddingBlock $ Padding len

data MetadataBlockHeader = MetadataBlockHeader
  { blockType :: Word8
  , blockLength :: Word32
  , isLast :: Bool
  } deriving (Show)

instance Binary MetadataBlockHeader where
  put = undefined
  get = do
    byte <- getWord8
    let isLast = testBit byte 7
    let blockType = clearBit byte 7
    len <- get24Bits
    let header = MetadataBlockHeader blockType len isLast
    traceM $ show header
    return header

data StreamInfo = StreamInfo
  { minBlockSize :: Word16
  , maxBlockSize :: Word16
  , minFrameSize :: Maybe Word32
  , maxFrameSize :: Maybe Word32
  , sampleRate :: Word32
  , channels :: Word8
  , bps :: Word8
  , samples :: Maybe Word64
  , md5 :: L.ByteString
  } deriving (Show)

instance Binary StreamInfo where
  put = undefined
  get = do
    header <- get
    guard $ blockType header == 0
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
        { minBlockSize
        , maxBlockSize
        , minFrameSize = mfilter (> 0) $ Just minFrameSize
        , maxFrameSize = mfilter (> 0) $ Just maxFrameSize
        , sampleRate
        , channels
        , bps
        , samples = mfilter (> 0) $ Just samples
        , md5
        }

data Padding =
  Padding Word32
  deriving (Show)

instance Binary Padding where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    guard $ blockType header == 1
    let paddingLength = blockLength header
    traceM $ "found padding; skipping " ++ (show paddingLength)
    skip $ fromIntegral $ paddingLength
    return $ Padding paddingLength

data Application = Application
  { applicationId :: Word32
  , applicationData :: L.ByteString
  } deriving (Show)

instance Binary Application where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    guard $ blockType header == 2
    Application <$> getWord32be <*>
      (getLazyByteString $ fromIntegral (blockLength header))

data SeekTable =
  SeekTable [SeekPoint]
  deriving (Show)

instance Binary SeekTable where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    guard $ blockType header == 3
    let numPoints = fromIntegral $ (blockLength header) `div` 18
    SeekTable <$> replicateM numPoints get

data SeekPoint = SeekPoint
  { firstSample :: Word64
  , byteOffset :: Word64
  , samples :: Word16
  } deriving (Show)

instance Binary SeekPoint where
  put = undefined
  get = do
    SeekPoint <$> getWord64be <*> getWord64be <*> getWord16be

data CueSheet = CueSheet
  { catalogNum :: L.ByteString
  , leadInSamples :: Word64
  , isCD :: Bool
  , tracks :: [CueSheetTrack]
  } deriving (Show)

instance Binary CueSheet where
  put = undefined
  get = do
    catalogNum <- getLazyByteString 128
    leadInSamples <- getWord64be
    a <- getWord8
    let isCD = testBit a 7
    skip 258
    numTracks <- fromIntegral <$> mfilter (> 1) getWord8
    tracks <- replicateM numTracks get
    return $ CueSheet {catalogNum, leadInSamples, isCD, tracks}

data CueSheetTrack = CueSheetTrack
  { sampleOffset :: Word64
  , trackNumber :: Word8
  , isrc :: L.ByteString
  , isAudio :: Bool
  , preEmphasis :: Bool
  , indexPoints :: [CueSheetTrackIndex]
  } deriving (Show)

instance Binary CueSheetTrack where
  put = undefined
  get = do
    sampleOffset <- getWord64be
    trackNumber <- getWord8
    isrc <- getLazyByteString 12
    a <- getWord8
    let isAudio = testBit a 7
    let preEmphasis = testBit a 6
    skip 13
    numIndexPoints <- fromIntegral <$> getWord8
    indexPoints <- replicateM numIndexPoints get
    return $
      CueSheetTrack
        {sampleOffset, trackNumber, isrc, isAudio, preEmphasis, indexPoints}

data CueSheetTrackIndex = CueSheetTrackIndex
  { sampleOffset :: Word64
  , indexPoint :: Word8
  } deriving (Show)

instance Binary CueSheetTrackIndex where
  put = undefined
  get = do
    sampleOffset <- getWord64be
    indexPoint <- getWord8
    skip 3
    return $ CueSheetTrackIndex sampleOffset indexPoint

get24Bits :: (Num a, Bits a) => Get a
get24Bits = do
  x <- liftM fromIntegral getWord8
  y <- liftM fromIntegral getWord8
  z <- liftM fromIntegral getWord8
  return $ (shiftL x 16) .|. (shiftL y 8) .|. z
