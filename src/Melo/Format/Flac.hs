module Melo.Format.Flac where

import Control.Monad
import Data.Binary
import Data.Binary.Bits.Get ()
import qualified Data.Binary.Bits.Get as BG
import Data.Binary.Get
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Text
import Debug.Trace
import Text.Printf

import Melo.Internal.BinaryUtil
import Melo.Format.Vorbis

readFlac :: FilePath -> IO FlacStream
readFlac p = decode <$> L.readFile p

readFlacOrFail :: FilePath -> IO (Either (ByteOffset, String) FlacStream)
readFlacOrFail = decodeFileOrFail

data Flac
  = Flac FlacStream
  | FlacWithId3v2 Int
                  FlacStream

data FlacStream = FlacStream
  { streamInfoBlock :: StreamInfo
  , metadataBlocks :: [MetadataBlock]
  } deriving (Show)

instance Binary FlacStream where
  put = undefined
  get = do
    marker <- getByteString 4
    expect ("fLaC" == marker) "Expected marker `fLaC`"
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
  | ReservedBlock ByteString
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
        let len = blockLength header'
        skip $ fromIntegral len
        ReservedBlock <$> getByteString (fromIntegral len)

data MetadataBlockHeader = MetadataBlockHeader
  { blockType :: Word8
  , blockLength :: Word32
  , isLast :: Bool
  } deriving (Show)

instance Binary MetadataBlockHeader where
  put = undefined
  get = do
    (isLast, blockType) <-
      BG.runBitGet $ (,) <$> BG.getBool <*> BG.getWord8 7
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
    expect (blockType header == 0) (printf "Unexpected block type %d; expected 0 (STREAMINFO)" $ blockType header)
    minBlockSize <- getWord16be
    expect (minBlockSize >= 16) ("Invalid min block size" ++ show minBlockSize)
    maxBlockSize <- getWord16be
    expect (maxBlockSize >= 16) ("Invalid max block size" ++ show maxBlockSize)
    minFrameSize <- get24Bits
    maxFrameSize <- get24Bits
    (sampleRate, channels, bps, samples) <-
      BG.runBitGet $ do
        sampleRate <- BG.getWord32be 20
        channels <- (+ 1) <$> BG.getWord8 3
        bps <- (+ 1) <$> BG.getWord8 5
        samples <- BG.getWord64be 36
        return (sampleRate, channels, bps, samples)
    expect (sampleRate > 0) ("Invalid sample rate" ++ show sampleRate)
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
    expect (blockType header == 1) (printf "Unexpected block type %d; expected 1 (PADDING)" $ blockType header)
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
    expect (blockType header == 2) (printf "Unexpected block type %d; expected 2 (APPLICATION)" $ blockType header)
    Application <$> getWord32be <*>
      getByteString (fromIntegral (blockLength header))

newtype SeekTable =
  SeekTable [SeekPoint]
  deriving (Show)

instance Binary SeekTable where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    expect (blockType header == 3) (printf "Unexpected block type %d; expected 3 (SEEKTABLE)" $ blockType header)
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

newtype FlacTags =
  FlacTags VorbisComments
  deriving (Show)

instance Binary FlacTags where
  put = undefined
  get = do
    header <- get :: Get MetadataBlockHeader
    expect (blockType header == 4) (printf "Unexpected block type %d; expected 4 (VORBIS_COMMENT)" $ blockType header)
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
    expect (blockType header == 5) (printf "Unexpected block type %d; expected 5 (CUESHEET)" $ blockType header)
    catalogNum <- getUTF8Text 128
    leadInSamples <- getWord64be
    isCD <- BG.runBitGet BG.getBool
    skip 258
    numTracks <- fromIntegral <$> failFilter (> 1) "Invalid number of cue sheet tracks" getWord8
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
    (isAudio, preEmphasis) <-
      BG.runBitGet $ (,) <$> BG.getBool <*> BG.getBool
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
    expect (blockType header == 6) (printf "Unexpected block type %d; expected 6 (PICTURE)" $ blockType header)
    pictureType <- failFilter (<= 20) "Invalid picture type" getWord32be
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
