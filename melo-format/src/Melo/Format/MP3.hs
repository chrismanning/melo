module Melo.Format.MP3 where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import qualified Control.Monad.Fail as F
import Data.Binary
import Data.Binary.Bits
import qualified Data.Binary.Bits.Get as BG
import qualified Data.Binary.Bits.Put as BP
import Data.Binary.Put
import Data.ByteString hiding ((!?))
import qualified Data.ByteString.Lazy as L
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Text as T (pack)
import Data.Vector ((!?))
import qualified Data.Vector as V
import GHC.Generics
import Lens.Micro
import Melo.Format.Ape (APEv1 (..), APEv2 (..))
import Melo.Format.Error
import qualified Melo.Format.ID3 as ID3
import qualified Melo.Format.Internal.Info as I
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import System.Directory
import System.FilePath
import System.IO

data MP3 = MP3
  { frameHeader :: !FrameHeader,
    id3v1 :: !(Maybe ID3.ID3v1),
    id3v2_3 :: !(Maybe ID3.ID3v2_3),
    id3v2_4 :: !(Maybe ID3.ID3v2_4),
    apev1 :: !(Maybe APEv1),
    apev2 :: !(Maybe APEv2)
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
      [ (\m -> (m.formatId, m)) . extractMetadata <$> apev2,
        (\m -> (m.formatId, m)) . extractMetadata <$> apev1,
        (\m -> (m.formatId, m)) . extractMetadata <$> id3v2_4,
        (\m -> (m.formatId, m)) . extractMetadata <$> id3v2_3,
        (\m -> (m.formatId, m)) . extractMetadata <$> id3v1
      ]

readMp3File :: FilePath -> IO MetadataFile
readMp3File p = do
  mp3 <- withBinaryFile p ReadMode hReadMp3
  pure
    MetadataFile
      { metadata = mp3Metadata mp3,
        audioInfo = I.info mp3,
        fileId = mp3FileKind,
        filePath = p
      }

hReadMp3 :: Handle -> IO MP3
hReadMp3 h = do
  seekable <- hIsSeekable h
  if seekable
    then do
      id3v2_3 <- hLocateGet' @ID3.ID3v2_3 h
      id3v2_4 <- hLocateGet' @ID3.ID3v2_4 h
      apev2 <- hLocateGet' @APEv2 h
      apev1 <- hLocateGet' @APEv1 h
      id3v1 <- hLocateGet' @ID3.ID3v1 h
      hLocateGet' @FrameHeader h >>= \case
        Just frameHeader ->
          pure
            MP3
              { frameHeader,
                id3v1,
                id3v2_3,
                id3v2_4,
                apev2,
                apev1
              }
        Nothing -> throwIO $ MetadataReadError "Unable to read MPEG frame header"
    else F.fail "Handle not seekable"

writeMp3File :: MetadataFile -> FilePath -> IO ()
writeMp3File f newpath = do
  oldpath <- canonicalizePath $ f ^. #filePath
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
            quality = Just $ T.pack $ show bitRate,
            -- TODO total mp3 samples
            totalSamples = Nothing,
            bitsPerSample = Nothing
          }
    where
      convertChannels :: Channels -> I.Channels
      convertChannels Stereo = I.Stereo
      convertChannels JointStereo = I.JointStereo
      convertChannels DualChannel = I.Stereo
      convertChannels Mono = I.Mono

data FrameHeader = FrameHeader
  { mpegAudioVersion :: !MpegVersion,
    layer :: !Layer,
    bitRate :: !BitRate,
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
    BP.putBool (isJust crc)
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
        protected <- BG.getBool
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

instance MetadataLocator FrameHeader where
  hLocate h =
    hLocateGet' @ID3.ID3v2_3 h >>= \case
      Just id3v2 ->
        findHeader (metadataSize id3v2)
      Nothing ->
        hLocateGet' @ID3.ID3v2_4 h >>= \case
          Just id3v2 -> findHeader (metadataSize id3v2)
          Nothing ->
            findHeader 0
    where
      findHeader pos = do
        hSeek h AbsoluteSeek pos
        bs <- L.hGet h (4 * 1024)
        case locateBinaryLazy @FrameHeader bs of
          Just n -> pure $ Just (fromIntegral (n + fromInteger pos))
          Nothing -> pure Nothing

data MpegVersion = V2_5 | V2 | V1
  deriving (Show, Eq, Ord, Generic)

instance Hashable MpegVersion

getMpegVersion :: BG.BitGet MpegVersion
getMpegVersion =
  BG.getWord8 2 >>= \case
    0b00 -> pure V2_5
    0b01 -> F.fail "reserved MPEG version ID"
    0b10 -> pure V2
    0b11 -> pure V1
    _ -> error "unreachable"

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
    0b00 -> F.fail "reserved MPEG version ID"
    0b01 -> pure Layer3
    0b10 -> pure Layer2
    0b11 -> pure Layer1
    _ -> error "unreachable"

putLayer :: Layer -> BP.BitPut ()
putLayer Layer1 = BP.putWord8 2 0b11
putLayer Layer2 = BP.putWord8 2 0b10
putLayer Layer3 = BP.putWord8 2 0b01

data BitRate = VBR !Int | CBR !Int
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

getBitRate :: MpegVersion -> Layer -> BG.BitGet BitRate
getBitRate version layer = do
  idx <- fromIntegral <$> BG.getWord8 4
  case H.lookup (version, layer) bitRateIndex >>= (!? idx) of
    Nothing -> error "invalid bitrate index"
    -- TODO VBR
    Just bitrate -> pure (CBR bitrate)

putBitRate :: BitRate -> MpegVersion -> Layer -> BP.BitPut ()
-- TODO VBR
putBitRate (VBR _) _ _ = error "VBR support not implemented"
putBitRate (CBR bitRate) version layer = do
  let bitRates = bitRateIndex ! (version, layer)
  case V.elemIndex bitRate bitRates of
    Just idx -> BP.putWord8 4 (fromIntegral idx)
    Nothing ->
      error $
        "invalid bit rate " <> show bitRate

data Channels = Stereo | JointStereo | DualChannel | Mono
  deriving (Show, Eq)

getChannels :: BG.BitGet Channels
getChannels =
  BG.getWord8 2 >>= pure <$> \case
    0b00 -> Stereo
    0b01 -> JointStereo
    0b10 -> DualChannel
    0b11 -> Mono
    _ -> error "unreachable"

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
    0b11 -> error "reserved sample rate"
    _ -> error "unreachable"

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
  error $
    "invalid sample rate " <> show sampleRate <> " for MPEG " <> show version
