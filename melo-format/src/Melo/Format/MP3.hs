module Melo.Format.MP3 where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Fail qualified as F
import Data.Binary
import Data.Binary.Bits
import Data.Binary.Bits.Get qualified as BG
import Data.Binary.Bits.Put qualified as BP
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString hiding ((!?))
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Coerce
import Data.HashMap.Strict ((!))
import Data.HashMap.Strict qualified as H
import Data.Hashable
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Text qualified as T (pack)
import Data.Vector ((!?))
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Lens.Micro
import Melo.Format.Ape (APEv1 (..), APEv2 (..), hGetApe)
import Melo.Format.Ape qualified as Ape
import Melo.Format.Error
import Melo.Format.ID3 qualified as ID3
import Melo.Format.Internal.Info qualified as I
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Streaming.ByteString qualified as S
import System.Directory
import System.FilePath
import System.IO
import Witch

data MP3 = MP3
  { frameHeader :: !FrameHeader,
    id3v1 :: !(Maybe ID3.ID3v1),
    id3v2_3 :: !(Maybe ID3.ID3v2_3),
    id3v2_4 :: !(Maybe ID3.ID3v2_4),
    apev1 :: !(Maybe APEv1),
    apev2 :: !(Maybe APEv2),
    samples :: !(Maybe Integer)
  }
  deriving (Show, Eq)

mp3 :: MetadataFileFactory IO
mp3 =
  MetadataFileFactory
    { priority = 100,
      fileId = mp3FileKind,
      detectFile = \p -> pure (takeExtension p == ".mp3"),
      readMetadataFile = readMp3File,
      writeMetadataFile = writeMp3File
    }

mp3FileKind :: MetadataFileId
mp3FileKind = MetadataFileId "MP3"

mp3Metadata :: MP3 -> H.HashMap MetadataId Metadata
mp3Metadata MP3 {..} =
  H.fromList $
    catMaybes
      [ asMetadata <$> apev2,
        asMetadata <$> apev1,
        asMetadata <$> id3v2_4,
        asMetadata <$> id3v2_3,
        asMetadata <$> id3v1
      ]
  where
    asMetadata :: MetadataFormat m => m -> (MetadataId, Metadata)
    asMetadata = toPair . extractMetadata
    toPair m = (m.formatId, m)

mp3Pictures :: MP3 -> [(PictureType, EmbeddedPicture)]
mp3Pictures MP3 {id3v2_3, id3v2_4} =
  let pictures = (ID3.id3v2Pictures <$> id3v2_4) `cc` (ID3.id3v2Pictures <$> id3v2_3) in
  pictures <&> \p -> (toEnum $ fromIntegral p.pictureType, from p)
    where
      cc :: Maybe [a] -> Maybe [a] -> [a]
      cc a b = fromMaybe [] a <> fromMaybe [] b

readMp3File :: FilePath -> IO MetadataFile
readMp3File p = do
  mp3 <- withBinaryFile p ReadMode hReadMp3
  pure
    MetadataFile
      { metadata = mp3Metadata mp3,
        audioInfo = I.info mp3,
        fileId = mp3FileKind,
        filePath = p,
        pictures = mp3Pictures mp3
      }

hReadMp3 :: Handle -> IO MP3
hReadMp3 h = do
  seekable <- hIsSeekable h
  if seekable
    then do
      id3v2_3 <- ID3.hGetId3v2 @'ID3.ID3v23 h
      id3v2_4 <- ID3.hGetId3v2 @'ID3.ID3v24 h
      apev2 <- hGetApe @'Ape.V2 h
      apev1 <- hGetApe @'Ape.V1 h
      id3v1 <- ID3.hGetId3v1 h
      hSeek h AbsoluteSeek 0
      ID3.hSkip h
      findBinary (get @FrameHeader) (S.hGetContents h) <&> snd >>= \case
        Just frameHeader -> do
          hSeek h AbsoluteSeek 0
          samples <- mp3Samples h
          pure
            MP3
              { frameHeader,
                id3v1,
                id3v2_3,
                id3v2_4,
                apev2,
                apev1,
                samples = if samples > 0 then Just samples else Nothing
              }
        Nothing -> throwIO $ MetadataReadError "Unable to read MPEG frame header"
    else F.fail "Handle not seekable"

writeMp3File :: MetadataFile -> FilePath -> IO ()
writeMp3File f newpath = do
  oldpath <- canonicalizePath f.filePath
  newpath <- canonicalizePath newpath
  if oldpath == newpath
    then do
      (tmpfile, h) <- openBinaryTempFile (takeDirectory newpath) (takeBaseName newpath <> ".tmp")
      hClose h
      writeMp3File' oldpath tmpfile
      copyPermissions oldpath tmpfile
      renameFile tmpfile newpath
    else writeMp3File' oldpath newpath
  where
    writeMp3File' oldpath newpath = do
      !mp3 <- withBinaryFile oldpath ReadMode hReadMp3
      -- TODO update mp3 from f
      !audioData <- withBinaryFile oldpath ReadMode $ \h -> do
        hSeek h SeekFromEnd 0
        end <- hTell h
        hSeek h AbsoluteSeek (mp3Size mp3)
        hGet h $ fromInteger (end - mp3Size mp3)
      withBinaryFile newpath WriteMode $ \h -> do
        hWriteMp3 h mp3
        hPut h audioData
    mp3Size :: MP3 -> Integer
    mp3Size MP3 {..} =
      (if isJust (crc frameHeader) then 6 else 4)
        + fromMaybe
          0
          ( fmap metadataSize id3v2_4
              <|> fmap metadataSize id3v2_3
              <|> fmap metadataSize apev2
          )

hWriteMp3 :: Handle -> MP3 -> IO ()
hWriteMp3 h mp3 = do
  let buf = L.toStrict $ runPut (putMp3 mp3)
  hPut h buf
  where
    putMp3 MP3 {..} = do
      let (<!|>) = liftM2 (<|>)
      _ <- mapM put id3v2_4 <!|> mapM put id3v2_3 <!|> mapM put apev2
      put frameHeader

instance I.InfoReader MP3 where
  info mp3 =
    let FrameHeader {..} = frameHeader mp3
     in I.Info
          { sampleRate = I.SampleRate (fromIntegral sampleRate),
            channels = convertChannels channels,
            quality = Just ((T.pack $ show @Int $ coerce bitRate) <> " kbps"),
            totalSamples = mp3.samples,
            bitsPerSample = Nothing
          }
    where
      convertChannels :: Channels -> I.Channels
      convertChannels Stereo = I.Stereo
      convertChannels JointStereo = I.JointStereo
      convertChannels DualChannel = I.Stereo
      convertChannels Mono = I.Mono

data FrameDuration
  = MP3FrameSamples Integer
  | XingHeaderSamples Integer

mp3Samples :: Handle -> IO Integer
mp3Samples h = loop h 0
  where
    loop h total =
      nextFrameDuration h >>= \case
        Just (MP3FrameSamples s) -> loop h (total + s)
        Just (XingHeaderSamples s) -> pure s
        Nothing -> pure total
    nextFrameDuration h = do
      skipZeroes h
      Ape.hSkip h
      skipZeroes h
      ID3.hSkip h
      skipZeroes h
      buf <- L.hGet h 2
      if L.null buf
        then pure Nothing
        else
          if frameSync == (runGet (BG.runBitGet $ getBits 11) buf)
            then do
              hSeek h RelativeSeek (negate (fromIntegral $ L.length buf))
              buf <- L.hGet h 6
              case runGet (tryGetHeader) buf of
                Just header -> do
                  let headerDiff = if isJust header.crc then 0 else (-2)
                  let headerSize = fromIntegral (L.length buf) + headerDiff
                  mark <- hTell h <&> (+ headerDiff)
                  -- look for Xing/Lame header - offset never includes crc
                  hSeek h RelativeSeek (infoHeaderOffset header - 2)
                  info <- hGetSome h 4
                  if info == "Info" || info == "Xing" then do
                    flags <- hGetSome h 4
                    if ((flags `BS.index` 3) .&. 0b1) == 0b1 then do
                      totalFrames <- L.hGet h 4 <&> runGet getWord32be
                      let samples = samplesPerFrame header.mpegAudioVersion header.layer
                      pure $ Just (XingHeaderSamples (fromIntegral (totalFrames * samples)))
                    else do
                      findNextFrame header mark headerSize
                  else do
                    findNextFrame header mark headerSize
                Nothing -> pure Nothing
            else pure Nothing
    findNextFrame header mark headerSize = do
      let padding = if header.padding then 1 else 0
      let bitRate = (coerce header.bitRate) * 1000
      let samples = samplesPerFrame header.mpegAudioVersion header.layer
      let frameLength = (samples `div` 8) * bitRate `div` header.sampleRate + padding
      hSeek h AbsoluteSeek (mark + fromIntegral frameLength - headerSize)
      pure $ Just (MP3FrameSamples (fromIntegral samples))
    skipZeroes h = do
      buf <- hGetSome h 1
      if buf BS.!? 0 == Just 0
        then skipZeroes h
        else hSeek h RelativeSeek (fromIntegral (negate (BS.length buf)))
    infoHeaderOffset :: FrameHeader -> Integer
    infoHeaderOffset header =
      case header.mpegAudioVersion of
        V1 -> case header.channels of
          Mono -> 17
          _ -> 32
        _ -> case header.channels of
          Mono -> 9
          _ -> 17

data FrameHeader = FrameHeader
  { mpegAudioVersion :: !MpegVersion,
    layer :: !Layer,
    bitRate :: !FrameBitRate,
    sampleRate :: !Int,
    padding :: !Bool,
    private :: !Bool,
    channels :: !Channels,
    modeExtension :: !Word8,
    copyrighted :: !Bool,
    original :: !Bool,
    emphasis :: !Word8,
    crc :: !(Maybe Word16)
  }
  deriving (Show, Eq)

instance Binary FrameHeader where
  get =
    tryGetHeader >>= \case
      Just header' -> pure header'
      Nothing -> fail "Not MPEG frame"
  put FrameHeader {..} = BP.runBitPut $ do
    putBits 11 frameSync
    putMpegVersion mpegAudioVersion
    putLayer layer
    BP.putBool (isNothing crc)
    putBitRate bitRate mpegAudioVersion layer
    putSampleRate sampleRate mpegAudioVersion
    BP.putBool padding
    BP.putBool private
    putChannels channels
    BP.putWord8 2 modeExtension
    BP.putBool copyrighted
    BP.putBool original
    BP.putWord8 2 emphasis
    mapM_ (BP.putWord16be 16) crc

tryGetHeader :: Get (Maybe FrameHeader)
tryGetHeader =
  BG.runBitGet $ do
    frameSync' <- getBits 11
    if frameSync /= frameSync'
      then pure Nothing
      else do
        version <- getMpegVersion
        layer <- getLayer
        protected <- not <$> BG.getBool
        h <-
          FrameHeader version layer
            <$> getBitRate version layer
            <*> getSampleRate version
            <*> BG.getBool
            <*> BG.getBool
            <*> getChannels
            <*> BG.getWord8 2
            <*> BG.getBool
            <*> BG.getBool
            <*> BG.getWord8 2
            <*> getCrc16 protected
        pure (Just h)
  where
    getCrc16 :: Bool -> BG.BitGet (Maybe Word16)
    getCrc16 True = Just <$> BG.getWord16be 16
    getCrc16 False = pure Nothing

frameSync :: Word16
frameSync = 0b11111111111

data MpegVersion = V2_5 | V2 | V1
  deriving (Show, Eq, Ord, Generic)

instance Hashable MpegVersion

getMpegVersion :: BG.BitGet MpegVersion
getMpegVersion =
  BG.getWord8 2 >>= \case
    0b00 -> pure V2_5
    0b01 -> fail "reserved MPEG version ID"
    0b10 -> pure V2
    0b11 -> pure V1
    x -> fail $ "unknown MPEG version ID " <> show x

putMpegVersion :: MpegVersion -> BP.BitPut ()
putMpegVersion V1 = BP.putWord8 2 0b11
putMpegVersion V2 = BP.putWord8 2 0b10
putMpegVersion V2_5 = BP.putWord8 2 0b00

data Layer = Layer3 | Layer2 | Layer1
  deriving (Show, Eq, Ord, Generic)

instance Hashable Layer

getLayer :: BG.BitGet Layer
getLayer =
  BG.getWord8 2 >>= \case
    0b00 -> fail "reserved MPEG version ID"
    0b01 -> pure Layer3
    0b10 -> pure Layer2
    0b11 -> pure Layer1
    x -> fail $ "unknown MPEG layer ID " <> show x

putLayer :: Layer -> BP.BitPut ()
putLayer Layer1 = BP.putWord8 2 0b11
putLayer Layer2 = BP.putWord8 2 0b10
putLayer Layer3 = BP.putWord8 2 0b01

newtype FrameBitRate = FrameBitRate Int
  deriving (Show, Eq)

bitRateIndex :: H.HashMap (MpegVersion, Layer) (V.Vector Int)
bitRateIndex =
  H.fromList
    [ ((V1, Layer1), V.fromList [0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448]),
      ((V1, Layer2), V.fromList [0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384]),
      ((V1, Layer3), V.fromList [0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320]),
      ((V2, Layer1), V.fromList [0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256]),
      ((V2, Layer2), V.fromList [0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160]),
      ((V2, Layer3), V.fromList [0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160]),
      ((V2_5, Layer1), V.fromList [0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256]),
      ((V2_5, Layer2), V.fromList [0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160]),
      ((V2_5, Layer3), V.fromList [0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160])
    ]

getBitRate :: MpegVersion -> Layer -> BG.BitGet FrameBitRate
getBitRate version layer = do
  idx <- fromIntegral <$> BG.getWord8 4
  case H.lookup (version, layer) bitRateIndex >>= (!? idx) of
    Nothing -> fail $ "invalid bitrate index " <> show idx
    Just bitrate -> pure (FrameBitRate bitrate)

putBitRate :: FrameBitRate -> MpegVersion -> Layer -> BP.BitPut ()
putBitRate (FrameBitRate bitRate) version layer = do
  let bitRates = bitRateIndex ! (version, layer)
  case V.elemIndex bitRate bitRates of
    Just idx -> BP.putWord8 4 (fromIntegral idx)
    Nothing ->
      fail $
        "invalid bit rate " <> show bitRate

data Channels = Stereo | JointStereo | DualChannel | Mono
  deriving (Show, Eq)

getChannels :: BG.BitGet Channels
getChannels =
  BG.getWord8 2
    >>= \case
      0b00 -> pure Stereo
      0b01 -> pure JointStereo
      0b10 -> pure DualChannel
      0b11 -> pure Mono
      x -> fail $ "unknown channel index " <> show x

putChannels :: Channels -> BP.BitPut ()
putChannels Stereo = BP.putWord8 2 0b00
putChannels JointStereo = BP.putWord8 2 0b01
putChannels DualChannel = BP.putWord8 2 0b10
putChannels Mono = BP.putWord8 2 0b11

getSampleRate :: MpegVersion -> BG.BitGet Int
getSampleRate version =
  BG.getWord8 2 >>= \case
    0b00 -> case version of
      V1 -> pure 44100
      V2 -> pure 22050
      V2_5 -> pure 11025
    0b01 -> case version of
      V1 -> pure 48000
      V2 -> pure 24000
      V2_5 -> pure 12000
    0b10 -> case version of
      V1 -> pure 32000
      V2 -> pure 16000
      V2_5 -> pure 8000
    0b11 -> fail "reserved sample rate"
    x -> fail $ "unknown sample rate index " <> show x

putSampleRate :: Int -> MpegVersion -> BP.BitPut ()
putSampleRate 44100 V1 = BP.putWord8 2 0b00
putSampleRate 48000 V1 = BP.putWord8 2 0b01
putSampleRate 32000 V1 = BP.putWord8 2 0b10
putSampleRate 22050 V2 = BP.putWord8 2 0b00
putSampleRate 24000 V2 = BP.putWord8 2 0b01
putSampleRate 16000 V2 = BP.putWord8 2 0b10
putSampleRate 11025 V2_5 = BP.putWord8 2 0b00
putSampleRate 12000 V2_5 = BP.putWord8 2 0b01
putSampleRate 8000 V2_5 = BP.putWord8 2 0b10
putSampleRate sampleRate version =
  fail $
    "invalid sample rate " <> show sampleRate <> " for MPEG " <> show version

samplesPerFrame :: Integral a => MpegVersion -> Layer -> a
samplesPerFrame _ Layer1 = 384
samplesPerFrame _ Layer2 = 1152
samplesPerFrame V1 Layer3 = 1152
samplesPerFrame _ Layer3 = 576
