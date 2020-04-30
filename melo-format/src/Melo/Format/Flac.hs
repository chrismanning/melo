module Melo.Format.Flac
  ( Flac,
    pattern Flac,
    pattern FlacWithID3v2_3,
    pattern FlacWithID3v2_4,
    FlacStream,
    StreamInfo (..),
    streamInfoBlock,
    vorbisComment,
    hReadFlac,
    readFlacOrFail,
    removeID3,
    flacFileId,
    flac,
  )
where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Data.Binary
import Data.Binary.Bits.Get ()
import qualified Data.Binary.Bits.Get as BG
import Data.Binary.Get
import Data.ByteString hiding (unpack)
import qualified Data.ByteString.Lazy as L
import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import Data.Vector
import GHC.Records
import Lens.Micro
import Melo.Format.Error
import Melo.Format.ID3.ID3v2 hiding (Padding)
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Info
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Vorbis
import System.IO
import Text.Printf

hReadFlac :: Handle -> IO Flac
hReadFlac h = do
  hSeek h AbsoluteSeek 0
  flacLoc <- hFindFlac h
  case flacLoc of
    Nothing -> throwIO UnknownFormat
    Just x -> do
      hSeek h AbsoluteSeek 0
      buf <- hGetFileContents h
      case x of
        0 -> MkFlac <$> bdecodeOrThrowIO buf
        _ -> do
          let (id3buf, flacbuf) = L.splitAt (fromIntegral x) buf
          id3v23loc <- hLocate @ID3v2_3 h
          hSeek h AbsoluteSeek 0
          id3v24loc <- hLocate @ID3v2_4 h
          hSeek h AbsoluteSeek 0
          flac <- bdecodeOrThrowIO flacbuf
          if isJust id3v23loc
            then do
              id3 <- bdecodeOrThrowIO id3buf
              pure $ MkFlacWithID3v2_3 id3 flac
            else
              if isJust id3v24loc
                then do
                  id3 <- bdecodeOrThrowIO id3buf
                  pure $ MkFlacWithID3v2_4 id3 flac
                else error "ID3v2 detected but couldn't be read"

readFlacOrFail :: FilePath -> IO (Either (ByteOffset, String) Flac)
readFlacOrFail p = fmap MkFlac <$> bdecodeFileOrFail p

data Flac
  = MkFlac !FlacStream
  | MkFlacWithID3v2_3 !ID3v2_3 !FlacStream
  | MkFlacWithID3v2_4 !ID3v2_4 !FlacStream
  deriving (Show)

pattern Flac :: FlacStream -> Flac
pattern Flac flac = MkFlac flac

pattern FlacWithID3v2_3 :: ID3v2_3 -> FlacStream -> Flac
pattern FlacWithID3v2_3 id3v2 flac = MkFlacWithID3v2_3 id3v2 flac

pattern FlacWithID3v2_4 :: ID3v2_4 -> FlacStream -> Flac
pattern FlacWithID3v2_4 id3v2 flac = MkFlacWithID3v2_4 id3v2 flac

{-# COMPLETE Flac, FlacWithID3v2_3, FlacWithID3v2_4 #-}

flacFileId :: MetadataFileId
flacFileId = MetadataFileId "Flac"

flac :: MetadataFileFactory IO
flac =
  MetadataFileFactory
    { priority = 100,
      fileId = flacFileId,
      detectFile = \p -> withBinaryFile p ReadMode (fmap isJust . hFindFlac),
      readMetadataFile = \p -> do
        f <- withBinaryFile p ReadMode hReadFlac
        pure
          MetadataFile
            { metadata = flacMetadata f,
              audioInfo = info f,
              fileId = flacFileId,
              filePath = p
            }
    }

flacMetadata :: Flac -> H.HashMap MetadataId Metadata
flacMetadata (FlacWithID3v2_3 id3v2 f) =
  let id3v2fmt = metadataFormat @ID3v2_3
      vc = vorbisComment f
   in H.fromList $
        catMaybes
          [ Just (id3v2fmt ^. #formatId, extractMetadata id3v2),
            vc <&> (vorbisCommentsId,) . extractMetadata
          ]
flacMetadata (FlacWithID3v2_4 id3v2 f) =
  let id3v2fmt = metadataFormat @ID3v2_4
      vc = vorbisComment f
   in H.fromList $
        catMaybes
          [ Just (id3v2fmt ^. #formatId, extractMetadata id3v2),
            vc <&> (vorbisCommentsId,) . extractMetadata
          ]
flacMetadata (Flac f) =
  let vc = vorbisComment f
   in H.fromList $
        catMaybes
          [ vc <&> (vorbisCommentsId,) . extractMetadata
          ]

instance InfoReader Flac where
  info f = case f of
    Flac fs -> getInfo fs
    FlacWithID3v2_3 _ fs -> getInfo fs
    FlacWithID3v2_4 _ fs -> getInfo fs
    where
      getInfo :: FlacStream -> Info
      getInfo fs =
        let si = streamInfoBlock fs
         in Info
              { sampleRate = SampleRate $ fromIntegral (getField @"sampleRate" si),
                bitsPerSample = pure $ fromIntegral $ bps si,
                channels = case getField @"channels" si of
                  1 -> Mono
                  2 -> Stereo
                  _ -> MultiChannel ChannelMask,
                totalSamples = fromIntegral <$> getField @"samples" si,
                quality = Nothing -- TODO flac quality
              }

hFindFlac :: Handle -> IO (Maybe Integer)
hFindFlac h = do
  id3v24loc <- hLocate @ID3v2_4 h
  hSeek h AbsoluteSeek 0
  id3v23loc <- hLocate @ID3v2_3 h
  hSeek h AbsoluteSeek 0
  case (id3v23loc, id3v24loc) of
    (Nothing, Nothing) -> do
      buf <- hGet h 4
      pure $ case buf of
        "fLaC" -> Just 0
        _ -> Nothing
    (Just id3v23loc', Nothing) -> do
      hSeek h AbsoluteSeek (fromIntegral id3v23loc')
      id3 <- bdecode @ID3v2_3 <$> hGetFileContents h
      hSeek h AbsoluteSeek $ id3 ^. #id3v2size + fromIntegral headerSize
      flacLoc <- hTell h
      buf <- hGet h 4
      pure $ case buf of
        "fLaC" -> Just (fromIntegral flacLoc)
        _ -> Nothing
    (Nothing, Just id3v24loc') -> do
      hSeek h AbsoluteSeek (fromIntegral id3v24loc')
      id3 <- bdecode @ID3v2_4 <$> hGetFileContents h
      hSeek h AbsoluteSeek $ id3 ^. #id3v2size + fromIntegral headerSize
      flacLoc <- hTell h
      buf <- hGet h 4
      pure $ case buf of
        "fLaC" -> Just (fromIntegral flacLoc)
        _ -> Nothing
    (Just _, Just _) -> error "Cannot have both ID3v2.3 and ID3v2.4"

data FlacStream = FlacStream
  { streamInfoBlock :: !StreamInfo,
    metadataBlocks :: !(Vector MetadataBlock)
  }
  deriving (Show)

vorbisComment :: FlacStream -> Maybe VorbisComments
vorbisComment (FlacStream _ blocks) = findVcs $ toList blocks
  where
    findVcs [] = Nothing
    findVcs (m : ms) = case m of
      VorbisCommentBlock (FlacTags vcs) -> Just vcs
      _ -> findVcs ms

instance BinaryGet FlacStream where
  bget = do
    expectGetEq (getByteString 4) "fLaC" "Couldn't find fLaC marker"
    FlacStream <$> bget <*> getMetadataBlocks

getMetadataBlocks :: Get (Vector MetadataBlock)
getMetadataBlocks = fromList <$> go
  where
    go = do
      header <- lookAhead bget
      block <- bget
      if isLast header
        then pure [block]
        else do
          blocks <- go
          return $ block : blocks

data MetadataBlock
  = StreamInfoBlock !StreamInfo
  | PaddingBlock !Padding
  | VorbisCommentBlock !FlacTags
  | PictureBlock !Picture
  | OtherBlock !Word8 !Word32
  deriving (Show)

instance BinaryGet MetadataBlock where
  bget = do
    header <- lookAhead bget
    case blockType header of
      0 -> StreamInfoBlock <$> bget
      1 -> PaddingBlock <$> bget
      4 -> VorbisCommentBlock <$> bget
      6 -> PictureBlock <$> bget
      127 -> fail "Invalid flac block type 127"
      bt -> do
        header' <- bget
        let len = blockLength header'
        skip $ fromIntegral len
        pure $ OtherBlock bt len

data MetadataBlockHeader = MetadataBlockHeader
  { blockType :: !Word8,
    blockLength :: !Word32,
    isLast :: !Bool
  }
  deriving (Show)

instance BinaryGet MetadataBlockHeader where
  bget = do
    (isLast, blockType) <-
      BG.runBitGet $ (,) <$> BG.getBool <*> BG.getWord8 7
    blockLength <- get24Bits
    return MetadataBlockHeader {blockType, blockLength, isLast}

data StreamInfo = StreamInfo
  { minBlockSize :: !Word16,
    maxBlockSize :: !Word16,
    minFrameSize :: !(Maybe Word32),
    maxFrameSize :: !(Maybe Word32),
    sampleRate :: !Word32,
    channels :: !Word8,
    bps :: !Word8,
    samples :: !(Maybe Word64),
    md5 :: !ByteString
  }
  deriving (Show)

instance BinaryGet StreamInfo where
  bget = do
    header <- bget
    expect (blockType header == 0) (printf "Unexpected block type %d; expected 0 (STREAMINFO)" $ blockType header)
    minBlockSize <- getWord16be
    expect (minBlockSize >= 16) ("Invalid min block size" <> show minBlockSize)
    maxBlockSize <- getWord16be
    expect (maxBlockSize >= 16) ("Invalid max block size" <> show maxBlockSize)
    minFrameSize <- get24Bits
    maxFrameSize <- get24Bits
    (sampleRate, channels, bps, samples) <-
      BG.runBitGet $ do
        sampleRate <- BG.getWord32be 20
        channels <- (+ 1) <$> BG.getWord8 3
        bps <- (+ 1) <$> BG.getWord8 5
        samples <- BG.getWord64be 36
        return (sampleRate, channels, bps, samples)
    expect (sampleRate > 0) ("Invalid sample rate" <> show sampleRate)
    md5 <- getByteString 16
    return
      StreamInfo
        { minBlockSize,
          maxBlockSize,
          minFrameSize = mfilter (> 0) $ Just minFrameSize,
          maxFrameSize = mfilter (> 0) $ Just maxFrameSize,
          sampleRate,
          channels,
          bps,
          samples = mfilter (> 0) $ Just samples,
          md5
        }

newtype Padding = Padding Word32
  deriving (Show)

instance BinaryGet Padding where
  bget = do
    header <- bget
    expect (blockType header == 1) (printf "Unexpected block type %d; expected 1 (PADDING)" $ blockType header)
    let paddingLength = blockLength header
    skip $ fromIntegral paddingLength
    return $ Padding paddingLength

newtype FlacTags
  = FlacTags VorbisComments
  deriving (Show)

instance BinaryGet FlacTags where
  bget = do
    header <- bget
    expect (blockType header == 4) (printf "Unexpected block type %d; expected 4 (VORBIS_COMMENT)" $ blockType header)
    FlacTags <$> bget

data Picture = Picture
  { pictureType :: !Word32,
    mimeType :: !Text,
    description :: !Text,
    width :: !Word32,
    height :: !Word32,
    depth :: !Word32,
    numColours :: !(Maybe Word32),
    pictureData :: !ByteString
  }
  deriving (Show)

numColors :: Picture -> Maybe Word32
numColors = numColours

instance BinaryGet Picture where
  bget = do
    header <- bget
    expect (blockType header == 6) (printf "Unexpected block type %d; expected 6 (PICTURE)" $ blockType header)
    pictureType <- mfilter (<= 20) getWord32be <|> Fail.fail "Invalid picture type"
    mimeType <- getUTF8Text =<< fromIntegral <$> getWord32be
    description <- getUTF8Text =<< fromIntegral <$> getWord32be
    width <- getWord32be
    height <- getWord32be
    depth <- getWord32be
    numColours <- getWord32be
    pictureData <- getByteString =<< fromIntegral <$> getWord32be
    return $
      Picture
        { pictureType,
          mimeType,
          description,
          width,
          height,
          depth,
          numColours = mfilter (> 0) $ Just numColours,
          pictureData
        }

removeID3 :: Flac -> Flac
removeID3 f@(Flac _) = f
removeID3 (FlacWithID3v2_3 _ fs) = Flac fs
removeID3 (FlacWithID3v2_4 _ fs) = Flac fs
