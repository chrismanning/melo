module Melo.Format.WavPack
  ( WavPack (..),
    WavPackInfo (..),
    WavPackTags (..),
    AudioType (..),
    Channels (..),
    wavPackFileKind,
    wavPack,
    hReadWavPack,
  )
where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Vector.Primitive
import Data.Word
import GHC.Records
import Lens.Micro
import qualified Melo.Format.Ape as Ape
import qualified Melo.Format.ID3 as ID3
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Info
  ( Info (..),
    InfoReader (..),
  )
import qualified Melo.Format.Internal.Info as I
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Numeric.Natural (Natural)
import System.IO

wavPack :: MetadataFileFactory IO
wavPack =
  MetadataFileFactory
    { priority = 100,
      fileId = wavPackFileKind,
      detectFile = \p -> withBinaryFile p ReadMode $ \h -> do
        hSeek h AbsoluteSeek 0
        buf <- BS.hGet h 4
        pure $ buf == ckId,
      readMetadataFile = readWavPackFile,
      writeMetadataFile = writeWavPackFile
    }

wavPackFileKind :: MetadataFileId
wavPackFileKind = MetadataFileId "WavPack"

readWavPackFile :: FilePath -> IO MetadataFile
readWavPackFile p = do
  wv <- withBinaryFile p ReadMode hReadWavPack
  pure
    MetadataFile
      { metadata = wavPackMetadata wv,
        audioInfo = info wv,
        fileId = wavPackFileKind,
        filePath = p
      }

wavPackMetadata :: WavPack -> H.HashMap MetadataId Metadata
wavPackMetadata wv = case wavPackTags wv of
  NoTags -> H.empty
  JustAPE ape -> H.singleton (metadataFormat @Ape.APEv2 ^. #formatId) (extractMetadata ape)
  JustID3v1 id3v1 -> H.singleton (metadataFormat @ID3.ID3v1 ^. #formatId) (extractMetadata id3v1)
  Both ape id3v1 ->
    H.fromList
      [ (metadataFormat @Ape.APEv2 ^. #formatId, extractMetadata ape),
        (metadataFormat @ID3.ID3v1 ^. #formatId, extractMetadata id3v1)
      ]

writeWavPackFile :: MetadataFile -> FilePath -> IO ()
writeWavPackFile f p = do
  error "unimplemented"

data WavPack = WavPack
  { wavPackInfo :: !WavPackInfo,
    wavPackTags :: !WavPackTags
  }
  deriving (Show)

instance InfoReader WavPack where
  info wv =
    let WavPackInfo {..} = wavPackInfo wv
     in Info
          { sampleRate = I.SampleRate $ fromIntegral $ fromMaybe 0 sampleRate,
            channels = case channels of
              Mono -> I.Mono
              Stereo -> I.Stereo
              JointStereo -> I.JointStereo
              MultiChannel _ -> I.MultiChannel I.ChannelMask,
            totalSamples = fromIntegral <$> totalSamples,
            bitsPerSample = Just $ fromIntegral sampleSize,
            quality = Nothing -- TODO wavpack quality
          }

data WavPackInfo = WavPackInfo
  { totalSamples :: !(Maybe Word64),
    sampleSize :: !Word8,
    channels :: !Channels,
    sampleRate :: !(Maybe Word32),
    audioType :: !AudioType
  }
  deriving (Show, Eq)

instance MetadataLocator WavPackInfo where
  hLocate h = do
    hSeek h AbsoluteSeek 0
    buf <- BS.hGet h 32
    return $ case runGetOrFail get (L.fromStrict buf) of
      Right (_, _, _ :: WavPackInfo) -> Just 0
      Left _ -> Nothing

instance Binary WavPackInfo where
  get = do
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
  put i = do
    error "unimplemented"

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

sampleRates :: Vector Word32
sampleRates =
  fromList
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
        then sampleRates !? fromIntegral rateIdx
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
  | JustAPE !Ape.APEv2
  | JustID3v1 !ID3.ID3v1
  | Both !Ape.APEv2 !ID3.ID3v1
  deriving (Show, Eq)

instance Binary WavPackTags where
  get = do
    what <- lookAhead $ getByteString 8
    if BS.isPrefixOf Ape.preamble what
      then do
        ape <- get :: Get Ape.APEv2
        isEmpty >>= \case
          True -> return $ JustAPE ape
          False -> Both ape <$> get
      else
        if BS.isPrefixOf ID3.iD3v1Id what
          then JustID3v1 <$> get
          else return NoTags
  put wv = do
    error "unimplemented"

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

findApe :: Handle -> IO (Maybe Natural)
findApe h = do
  footerpos <- findAt h (- Ape.headerSize) Ape.preamble
  case footerpos of
    Just n -> do
      hSeek h AbsoluteSeek $ fromIntegral n
      bs <- BS.hGet h Ape.headerSize
      let footer = runGet Ape.getHeader (L.fromStrict bs)
      return $
        Just $
          if Ape.isHeader (Ape.flags footer)
            then fromIntegral n
            else fromIntegral n - fromIntegral (Ape.numBytes footer)
    Nothing -> return Nothing

findID3 :: Handle -> IO (Maybe Natural)
findID3 h = findAt h (-128) ID3.iD3v1Id

findAt :: Handle -> Integer -> BS.ByteString -> IO (Maybe Natural)
findAt h p s = do
  hSeek h RelativeSeek p
  pos <- hTell h
  s' <- BS.hGet h (BS.length s)
  if s' == s then return $ fromIntegral <$> Just pos else return Nothing

hReadWavPack :: Handle -> IO WavPack
hReadWavPack h = do
  wvInfo <- hLocateGet h
  wvTags <- do
    seekable <- hIsSeekable h
    if seekable
      then do
        hSeek h AbsoluteSeek 0
        hLocateGet h
      else return NoTags
  return $ WavPack wvInfo wvTags
