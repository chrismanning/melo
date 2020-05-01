module Melo.Format.MP3 where

import qualified Control.Monad.Fail as F
import Data.Binary
import Data.Binary.Bits
import qualified Data.Binary.Bits.Get as BG
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Either (isRight)
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.Maybe (catMaybes)
import qualified Data.Text as T (pack)
import Data.Vector as V
import GHC.Generics
import Lens.Micro
import Melo.Format.Ape (APEv1 (..), APEv2 (..))
import qualified Melo.Format.ID3 as ID3
import Melo.Format.Internal.Binary
import qualified Melo.Format.Internal.Info as I
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import System.FilePath (takeExtension)
import System.IO

mp3 :: MetadataFileFactory IO
mp3 =
  MetadataFileFactory
    { priority = 100,
      fileId = mp3FileKind,
      readMetadataFile = \p -> do
        mp3 <- withBinaryFile p ReadMode hReadMp3
        pure
          MetadataFile
            { metadata = mp3Metadata mp3,
              audioInfo = I.info mp3,
              fileId = mp3FileKind,
              filePath = p
            },
      detectFile = \p -> pure (takeExtension p == ".mp3")
    }

mp3FileKind :: MetadataFileId
mp3FileKind = MetadataFileId "MP3"

mp3Metadata :: MP3 -> H.HashMap MetadataId Metadata
mp3Metadata MP3 {..} =
  H.fromList $
    catMaybes
      [ (\m -> (m ^. #formatId, m)) . extractMetadata <$> apev2,
        (\m -> (m ^. #formatId, m)) . extractMetadata <$> apev1,
        (\m -> (m ^. #formatId, m)) . extractMetadata <$> id3v2_4,
        (\m -> (m ^. #formatId, m)) . extractMetadata <$> id3v2_3,
        (\m -> (m ^. #formatId, m)) . extractMetadata <$> id3v1
      ]

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
      Just frameHeader <- hLocateGet' @FrameHeader h
      pure
        MP3
          { frameHeader,
            id3v1,
            id3v2_3,
            id3v2_4,
            apev2,
            apev1
          }
    else F.fail "Handle not seekable"

data MP3 = MP3
  { frameHeader :: FrameHeader,
    id3v1 :: Maybe ID3.ID3v1,
    id3v2_3 :: Maybe ID3.ID3v2_3,
    id3v2_4 :: Maybe ID3.ID3v2_4,
    apev1 :: Maybe APEv1,
    apev2 :: Maybe APEv2
  }
  deriving (Show)

instance I.InfoReader MP3 where
  info mp3 =
    let FrameHeader {..} = frameHeader mp3
     in I.Info
          { sampleRate = I.SampleRate (fromIntegral sampleRate),
            channels = convertChannels channels,
            quality = Just $ T.pack $ show bitrate,
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
  { mpegAudioVersion :: MpegVersion,
    layer :: Layer,
    bitrate :: BitRate,
    sampleRate :: Int,
    channels :: Channels
  }
  deriving (Show, Eq)

instance BinaryGet FrameHeader where
  bget = tryGetHeader >>= \case
    Just header' -> pure header'
    Nothing -> fail "Not MPEG frame"

tryGetHeader :: Get (Maybe FrameHeader)
tryGetHeader = isolate 4 $ BG.runBitGet $ do
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
          <*> getChannels
          <* BG.getWord8 8
      pure (Just h)
  where
    frameSync :: Word32
    frameSync = 0b11111111111

instance MetadataLocator FrameHeader where
  hLocate h =
    hLocateGet' @ID3.ID3v2_3 h >>= \case
      Just ID3.ID3v2_3 {id3v2size} ->
        findHeader (id3v2size + fromIntegral ID3.headerSize)
      Nothing -> hLocateGet' @ID3.ID3v2_4 h >>= \case
        Just ID3.ID3v2_4 {id3v2size} ->
          findHeader (id3v2size + fromIntegral ID3.headerSize)
        Nothing ->
          findHeader 0
    where
      findHeader pos = do
        hSeek h AbsoluteSeek pos
        bs <- L.hGetContents h
        if isRight $ bdecodeOrFail @FrameHeader bs
          then pure $ Just (fromIntegral pos)
          else pure Nothing

data MpegVersion = V2_5 | V2 | V1
  deriving (Show, Eq, Ord, Generic, Hashable)

getMpegVersion :: BG.BitGet MpegVersion
getMpegVersion = BG.getWord8 2 >>= \case
  0b00 -> pure V2_5
  0b01 -> F.fail "reserved MPEG version ID"
  0b10 -> pure V2
  0b11 -> pure V1
  _ -> error "unreachable"

data Layer = Layer3 | Layer2 | Layer1
  deriving (Show, Eq, Ord, Generic, Hashable)

getLayer :: BG.BitGet Layer
getLayer = BG.getWord8 2 >>= \case
  0b00 -> F.fail "reserved MPEG version ID"
  0b01 -> pure Layer3
  0b10 -> pure Layer2
  0b11 -> pure Layer1
  _ -> error "unreachable"

data BitRate = VBR Int | CBR Int
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

data Channels = Stereo | JointStereo | DualChannel | Mono
  deriving (Show, Eq)

getChannels :: BG.BitGet Channels
getChannels = BG.getWord8 2 >>= pure <$> \case
  0b00 -> Stereo
  0b01 -> JointStereo
  0b10 -> DualChannel
  0b11 -> Mono
  _ -> error "unreachable"

getSampleRate :: MpegVersion -> BG.BitGet Int
getSampleRate version = BG.getWord8 2 >>= \case
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
