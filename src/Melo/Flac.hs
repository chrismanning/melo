module Melo.Flac where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Text
import Debug.Trace

import Melo.BinaryUtil
import Melo.Vorbis

readFlac :: FilePath -> IO FlacStream
readFlac p = decode <$> L.readFile p

readFlacOrFail :: FilePath -> IO (Either (ByteOffset, String) FlacStream)
readFlacOrFail = decodeFileOrFail

data Flac = Flac FlacStream | FlacWithId3v2 Int FlacStream

data FlacStream = FlacStream
  { streamInfoBlock :: StreamInfo
  , metadataBlocks :: [MetadataBlock]
  } deriving (Show)

instance Binary FlacStream where
  put = undefined
  get = do
    marker <- getByteString 4
    mustMatch "fLaC" marker "Invalid flac stream"
    FlacStream <$> get <*> getMetadataBlocks

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
  | VorbisCommentBlock FlacTags
  | CueSheetBlock CueSheet
  | PictureBlock Picture
  | Reserved
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
      4 -> VorbisCommentBlock <$> get
      5 -> CueSheetBlock <$> get
      6 -> PictureBlock <$> get
      127 -> fail "Invalid flac block type 127"
      _ -> do
        header' <- get
        skip $ fromIntegral $ blockLength header'
        return $ Reserved

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
    blockLength <- get24Bits
    let header = MetadataBlockHeader {blockType, blockLength, isLast}
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
  , md5 :: ByteString
  } deriving (Show)

instance Binary StreamInfo where
  put = undefined
  get = do
    header <- get
    mustMatch 0 (blockType header) "Error parsing STREAMINFO"
    minBlockSize <- getWord16be
    mustSatisfy (>= 16) minBlockSize "Invalid min block size"
    maxBlockSize <- getWord16be
    mustSatisfy (>= 16) maxBlockSize "Invalid max block size"
    minFrameSize <- get24Bits
    maxFrameSize <- get24Bits
    rest <- getWord64be
    let sampleRate = fromIntegral (shiftR rest 44 .&. 0xFFFFF)
    mustSatisfy (> 0) sampleRate ("Invalid sample rate")
    let channels = fromIntegral (shiftR rest 41 .&. 0b111) + 1
    let bps = fromIntegral (shiftR rest 36 .&. 0b11111) + 1
    let samples = fromIntegral (rest .&. 0xFFFFFFFFF)
    md5 <- getByteString 16
    return
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

newtype Padding =
  Padding Word32
  deriving (Show)

instance Binary Padding where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    mustMatch 1 (blockType header) "Error parsing PADDING"
    let paddingLength = blockLength header
    traceM $ "found padding; skipping " ++ show paddingLength
    skip $ fromIntegral paddingLength
    return $ Padding paddingLength

data Application = Application
  { applicationId :: Word32
  , applicationData :: ByteString
  } deriving (Show)

instance Binary Application where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    mustMatch 2 (blockType header) "Error parsing APPLICATION"
    Application <$> getWord32be <*>
      getByteString (fromIntegral (blockLength header))

newtype SeekTable =
  SeekTable [SeekPoint]
  deriving (Show)

instance Binary SeekTable where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    mustMatch 3 (blockType header) "Error parsing SEEKTABLE"
    let numPoints = fromIntegral $ blockLength header `div` 18
    SeekTable <$> replicateM numPoints get

data SeekPoint = SeekPoint
  { firstSample :: Word64
  , byteOffset :: Word64
  , samples :: Word16
  } deriving (Show)

instance Binary SeekPoint where
  put = undefined
  get = SeekPoint <$> getWord64be <*> getWord64be <*> getWord16be

data FlacTags =
  FlacTags VorbisComments
  deriving (Show)

instance Binary FlacTags where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    mustMatch 4 (blockType header) "Error parsing VORBIS_COMMENT"
    FlacTags <$> get

data CueSheet = CueSheet
  { catalogNum :: Text
  , leadInSamples :: Word64
  , isCD :: Bool
  , tracks :: [CueSheetTrack]
  } deriving (Show)

instance Binary CueSheet where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    mustMatch 5 (blockType header) "Error parsing CUESHEET"
    catalogNum <- getUTF8Text 128
    leadInSamples <- getWord64be
    a <- getWord8
    let isCD = testBit a 7
    skip 258
    numTracks <- fromIntegral <$> mfilter (> 1) getWord8
    tracks <- replicateM numTracks get
    return CueSheet {catalogNum, leadInSamples, isCD, tracks}

data CueSheetTrack = CueSheetTrack
  { sampleOffset :: Word64
  , trackNumber :: Word8
  , isrc :: Text
  , isAudio :: Bool
  , preEmphasis :: Bool
  , indexPoints :: [CueSheetTrackIndex]
  } deriving (Show)

instance Binary CueSheetTrack where
  put = undefined
  get = do
    sampleOffset <- getWord64be
    trackNumber <- getWord8
    isrc <- getUTF8Text 12
    a <- getWord8
    let isAudio = testBit a 7
    let preEmphasis = testBit a 6
    skip 13
    numIndexPoints <- fromIntegral <$> getWord8
    indexPoints <- replicateM numIndexPoints get
    return
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

data Picture = Picture
  { pictureType :: Word32
  , mimeType :: Text
  , description :: Text
  , width :: Word32
  , height :: Word32
  , depth :: Word32
  , numColours :: Maybe Word32
  , pictureData :: ByteString
  } deriving (Show)

numColors :: Picture -> Maybe Word32
numColors = numColours

instance Binary Picture where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    mustMatch 6 (blockType header) "Error parsing PICTURE"
    pictureType <- mfilter (<= 20) getWord32be
    mimeType <- getUTF8Text =<< fromIntegral <$> getWord32be
    description <- getUTF8Text =<< fromIntegral <$> getWord32be
    width <- getWord32be
    height <- getWord32be
    depth <- getWord32be
    numColours <- getWord32be
    pictureData <- getByteString =<< fromIntegral <$> getWord32be
    return $
      Picture
        { pictureType
        , mimeType
        , description
        , width
        , height
        , depth
        , numColours = mfilter (> 0) $ Just numColours
        , pictureData
        }
