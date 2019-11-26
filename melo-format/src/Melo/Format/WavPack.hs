module Melo.Format.WavPack
  ( WavPack (..),
    WavPackInfo (..),
    WavPackTags (..),
    AudioType (..),
    Channels (..),
    ChannelMask (..),
    hReadWavPack,
  )
where

import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.List (genericIndex)
import Data.Maybe
import Data.Word
import GHC.Records
import qualified Melo.Format.Ape as Ape
import Melo.Format.Format
import qualified Melo.Format.ID3 as ID3
import qualified Melo.Format.ID3.ID3v1 as ID3
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Detect
import qualified Melo.Format.Internal.Info as I
import Melo.Format.Internal.Info
  ( Info (..),
    InfoReader (..),
  )
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Tag
import Melo.Format.Mapping as M
  ( FieldMappings (ape),
  )
import System.FilePath
import System.IO

data WavPack
  = WavPack
      { wavPackInfo :: !WavPackInfo,
        wavPackTags :: !WavPackTags
      }
  deriving (Show)

instance MetadataFormat WavPack where

  formatDesc = "WavPack"

  formatDesc' wv = "WavPack with " <> formatDesc' (wavPackTags wv)

instance TagReader WavPack where
  tags wv = tags $ wavPackTags wv

instance InfoReader WavPack where
  info wv =
    let wi = wavPackInfo wv
     in Info
          { sampleRate = I.SampleRate $ fromIntegral $ fromMaybe 0 $ getField @"sampleRate" wi,
            channels = case getField @"channels" wi of
              Mono -> I.Mono
              Stereo -> I.Stereo
              JointStereo -> I.JointStereo
              MultiChannel _ -> I.MultiChannel I.ChannelMask,
            totalSamples = fromIntegral <$> getField @"totalSamples" wi,
            bitsPerSample = Just $ fromIntegral $ sampleSize wi
          }

instance Detector WavPack where

  pathDetectFormat p
    | takeExtension p == ".wv" = Just detector
    | otherwise = Nothing

  hDetectFormat h = do
    hSeek h AbsoluteSeek 0
    buf <- BS.hGet h 4
    if buf == ckId
      then return $ Just detector
      else return Nothing

detector :: DetectedP
detector = mkDetected hReadWavPack M.ape

data WavPackInfo
  = WavPackInfo
      { totalSamples :: !(Maybe Word64),
        sampleSize :: !Word8,
        channels :: !Channels,
        sampleRate :: !(Maybe Word32),
        audioType :: !AudioType
      }
  deriving (Show, Eq)

instance MetadataFormat WavPackInfo where
  formatDesc = "WavPack metadata block"

instance MetadataLocator WavPackInfo where
  hLocate h = do
    hSeek h AbsoluteSeek 0
    buf <- BS.hGet h 32
    return $ case runGetOrFail bget (L.fromStrict buf) of
      Right (_, _, _ :: WavPackInfo) -> Just 0
      Left _ -> Nothing

instance BinaryGet WavPackInfo where
  bget = do
    expectGetEq (getByteString $ BS.length ckId) ckId "Expected ckId `wvpk`"
    skip 6
    blockIndex8 <- getWord8
    totalSamples8 <- getWord8
    totalSamples32 <- getWord32le
    blockIndex32 <- getWord32le
    let blockIndex = forty blockIndex8 blockIndex32
    skip 4
    flags <- getWord32le
    let totalSamples =
          if blockIndex == 0 && totalSamples32 /= complement 0
            then Just $ forty totalSamples8 totalSamples32
            else Nothing
    return $
      WavPackInfo
        { totalSamples,
          sampleSize = getSampleSize flags,
          channels = getChannels flags,
          sampleRate = getSampleRate flags,
          audioType = getAudioType flags
        }

forty :: Word8 -> Word32 -> Word64
forty u8 l32 =
  let l32' :: Word64 = fromIntegral l32
      u8' :: Word64 = fromIntegral u8
   in u8' `shiftL` 32 .|. l32'

getSampleSize :: Word32 -> Word8
getSampleSize flags = fromIntegral $ 8 + (8 * (flags .&. 0b11))

getChannels :: Word32 -> Channels
getChannels flags
  | testBit flags 2 = Mono
  | otherwise = Stereo

sampleRates :: [Word32]
sampleRates =
  [ 6000,
    8000,
    9600,
    11025,
    12000,
    16000,
    22050,
    24000,
    32000,
    44100,
    48000,
    64000,
    88200,
    96000,
    192000
  ]

getSampleRate :: Word32 -> Maybe Word32
getSampleRate flags =
  let rateIdx = (flags `shiftR` 23) .&. 0b1111
   in if rateIdx /= 0b1111
        then Just $ sampleRates `genericIndex` rateIdx
        else Nothing

getAudioType :: Word32 -> AudioType
getAudioType flags
  | testBit flags 31 = DSD
  | otherwise = PCM

ckId :: BS.ByteString
ckId = "wvpk"

data AudioType
  = PCM
  | DSD
  deriving (Show, Eq)

data Channels
  = Mono
  | Stereo
  | JointStereo
  | MultiChannel !ChannelMask
  deriving (Show, Eq)

data ChannelMask
  = ChannelMask

deriving instance Show ChannelMask

deriving instance Eq ChannelMask

data WavPackTags
  = NoTags
  | JustAPE !Ape.APE
  | JustID3v1 !ID3.ID3v1
  | Both Ape.APE !ID3.ID3v1
  deriving (Show, Eq)

instance BinaryGet WavPackTags where
  bget = do
    what <- lookAhead $ getByteString 8
    if BS.isPrefixOf Ape.preamble what
      then do
        ape <- bget :: Get Ape.APE
        isEmpty >>= \case
          True -> return $ JustAPE ape
          False -> Both ape <$> bget
      else
        if BS.isPrefixOf ID3.iD3v1Id what
          then JustID3v1 <$> bget
          else return NoTags

instance MetadataFormat WavPackTags where

  formatDesc = "APEv2 and/or ID3v1"

  formatDesc' NoTags = "empty tags"
  formatDesc' (JustAPE _) = "APEv2"
  formatDesc' (JustID3v1 _) = "ID3v1"
  formatDesc' (Both _ _) = "APEv2 + ID3v1"

instance MetadataLocator WavPackTags where
  hLocate h = do
    hSeek h SeekFromEnd 0
    findApe h >>= \case
      Just n -> return $ Just n
      Nothing -> do
        hSeek h SeekFromEnd 0
        findID3 h >>= \case
          Nothing -> do
            hSeek h AbsoluteSeek Ape.headerSize
            findApe h
          Just id3pos -> do
            hSeek h AbsoluteSeek $ fromIntegral id3pos
            findApe h >>= \case
              Nothing -> return $ Just id3pos
              Just apepos -> return $ Just apepos

instance TagReader WavPackTags where
  tags = \case
    JustAPE ape -> tags ape
    JustID3v1 id3v1 -> tags id3v1
    Both ape _ -> tags ape
    NoTags -> Tags []

findApe :: Handle -> IO (Maybe Int)
findApe h = do
  footerpos <- findAt h (- Ape.headerSize) Ape.preamble
  case footerpos of
    Just n -> do
      hSeek h AbsoluteSeek $ fromIntegral n
      bs <- BS.hGet h Ape.headerSize
      let footer = runGet Ape.getHeader (L.fromStrict bs)
      return $ Just $
        if Ape.isHeader (Ape.flags footer)
          then n
          else n - fromIntegral (Ape.numBytes footer)
    Nothing -> return Nothing

findID3 :: Handle -> IO (Maybe Int)
findID3 h = findAt h (-128) ID3.iD3v1Id

findAt :: Handle -> Integer -> BS.ByteString -> IO (Maybe Int)
findAt h p s = do
  hSeek h RelativeSeek p
  pos <- hTell h
  s' <- BS.hGet h (BS.length s)
  if s' == s then return $ fromIntegral <$> Just pos else return Nothing

hReadWavPack :: Handle -> IO WavPack
hReadWavPack h = do
  wvInfo <- hGetMetadata h
  wvTags <- do
    seekable <- hIsSeekable h
    if seekable
      then do
        hSeek h AbsoluteSeek 0
        hGetMetadata h
      else return NoTags
  return $ WavPack wvInfo wvTags
