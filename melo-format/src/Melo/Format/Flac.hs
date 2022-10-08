{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Flac
  ( Flac,
    pattern Flac,
    pattern FlacWithID3v2_3,
    pattern FlacWithID3v2_4,
    StreamInfo (..),
    streamInfo,
    vorbisComment,
    hReadFlac,
    readFlacOrFail,
    readFlacFile,
    writeFlacFile,
    removeID3,
    flacFileId,
    flac,
    Picture,
    numColors,
  )
where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Fail qualified as Fail
import Data.Binary
import Data.Binary.Bits.Get ()
import Data.Binary.Bits.Get qualified as BG
import Data.Binary.Bits.Put ()
import Data.Binary.Bits.Put qualified as BP
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L
import Data.Foldable
import Data.Generics.Labels ()
import Data.HashMap.Strict qualified as H
import Data.Maybe
import Data.Monoid
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import GHC.Records
import Lens.Micro
import Melo.Format.Error
import Melo.Format.ID3.ID3v2 as ID3v2 hiding (Picture)
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Info
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Vorbis
import System.Directory
import System.FilePath
import System.IO
import Witch

readFlacOrFail :: FilePath -> IO (Either (ByteOffset, String) Flac)
readFlacOrFail p = fmap MkFlac <$> bdecodeFileOrFail p

data Flac
  = MkFlac !FlacMetadata
  | MkFlacWithID3v2_3 !ID3v2_3 !FlacMetadata
  | MkFlacWithID3v2_4 !ID3v2_4 !FlacMetadata
  deriving (Show)

flacMetadata :: Flac -> FlacMetadata
flacMetadata (Flac m) = m
flacMetadata (FlacWithID3v2_3 _ m) = m
flacMetadata (FlacWithID3v2_4 _ m) = m

pattern Flac :: FlacMetadata -> Flac
pattern Flac flac = MkFlac flac

pattern FlacWithID3v2_3 :: ID3v2_3 -> FlacMetadata -> Flac
pattern FlacWithID3v2_3 id3v2 flac = MkFlacWithID3v2_3 id3v2 flac

pattern FlacWithID3v2_4 :: ID3v2_4 -> FlacMetadata -> Flac
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
      readMetadataFile = readFlacFile,
      writeMetadataFile = writeFlacFile
    }

flacPictures :: Flac -> [(PictureType, EmbeddedPicture)]
flacPictures flac =
  fmap
    (\p -> (toEnum $ fromIntegral p.pictureType, from p))
    (flacMetadataPictures (flacMetadata flac))

readFlacFile :: FilePath -> IO MetadataFile
readFlacFile p = do
  f <- withBinaryFile p ReadMode hReadFlac
  pure
    MetadataFile
      { metadata = collectFlacMetadata f,
        audioInfo = info f,
        fileId = flacFileId,
        filePath = p,
        pictures = flacPictures f
      }

writeFlacFile :: MetadataFile -> FilePath -> IO ()
writeFlacFile f newpath = do
  oldpath <- canonicalizePath $ f ^. #filePath
  newpath <- canonicalizePath newpath
  if oldpath == newpath
    then do
      -- TODO utilise padding when updating existing flac files
      (tmpfile, h) <- openBinaryTempFile (takeDirectory newpath) (takeBaseName newpath <> ".tmp")
      hClose h
      writeFlacFile' oldpath tmpfile
      copyPermissions oldpath tmpfile
      renameFile tmpfile newpath
    else writeFlacFile' oldpath newpath
  where
    writeFlacFile' oldpath newpath = do
      !oldflac <- withBinaryFile oldpath ReadMode hReadFlac
      !audioData <- withBinaryFile oldpath ReadMode $ \h -> do
        hSeek h SeekFromEnd 0
        end <- hTell h
        hSeek h AbsoluteSeek (flacSize oldflac)
        BS.hGet h $ fromInteger (end - flacSize oldflac)
      withBinaryFile newpath WriteMode $ \h -> do
        let !newflac = updateFlacWith (H.elems $ f ^. #metadata) oldflac
        hWriteFlac h newflac
        BS.hPut h audioData

updateFlacWith :: [Metadata] -> Flac -> Flac
updateFlacWith = flip $ foldl' replaceMetadata

replaceMetadata :: Flac -> Metadata -> Flac
replaceMetadata (Flac flacMetadata) Metadata {formatId, tags} = case formatId of
  fid | fid == id3v23Id -> MkFlacWithID3v2_3 (newId3v2 tags) flacMetadata
  fid | fid == id3v24Id -> MkFlacWithID3v2_4 (newId3v2 tags) flacMetadata
  fid
    | fid == vorbisCommentsId ->
        MkFlac (replaceVorbisCommentBlock flacMetadata tags)
  _otherwise -> impureThrow $ IncompatibleFormat flacFileId formatId
replaceMetadata (FlacWithID3v2_3 id3v23 flacMetadata) Metadata {formatId, tags} = case formatId of
  fid | fid == id3v23Id -> MkFlacWithID3v2_3 (replaceWithTags id3v23 tags) flacMetadata
  fid | fid == id3v24Id -> MkFlacWithID3v2_4 (replaceWithTags (changeVersion id3v23) tags) flacMetadata
  fid
    | fid == vorbisCommentsId ->
        MkFlacWithID3v2_3 id3v23 (replaceVorbisCommentBlock flacMetadata tags)
  _otherwise -> impureThrow $ IncompatibleFormat flacFileId formatId
replaceMetadata (FlacWithID3v2_4 id3v24 flacMetadata) Metadata {formatId, tags} = case formatId of
  fid | fid == id3v23Id -> MkFlacWithID3v2_3 (replaceWithTags (changeVersion id3v24) tags) flacMetadata
  fid | fid == id3v24Id -> MkFlacWithID3v2_4 (replaceWithTags id3v24 tags) flacMetadata
  fid
    | fid == vorbisCommentsId ->
        MkFlacWithID3v2_4 id3v24 (replaceVorbisCommentBlock flacMetadata tags)
  _otherwise -> impureThrow $ IncompatibleFormat flacFileId formatId

replaceVorbisCommentBlock :: FlacMetadata -> Tags -> FlacMetadata
replaceVorbisCommentBlock flacMetadata tags =
  flacMetadata
    & #metadataBlocks . mapped %~ \case
      VorbisCommentBlock isLast vcs ->
        VorbisCommentBlock isLast $
          replaceUserComments vcs (toUserComments tags)
      block -> block

flacSize :: Flac -> Integer
flacSize (Flac m) = flacMetadataSize m
flacSize (FlacWithID3v2_3 id3v23 m) = metadataSize id3v23 + flacMetadataSize m
flacSize (FlacWithID3v2_4 id3v24 m) = metadataSize id3v24 + flacMetadataSize m

flacMetadataSize :: FlacMetadata -> Integer
flacMetadataSize m =
  fromIntegral (BS.length marker)
    + blockHeaderSize
    + streamInfoSize
    + getSum (foldMap (Sum . metadataBlockSize) (metadataBlocks m))

hReadFlac :: Handle -> IO Flac
hReadFlac h = do
  hSeek h AbsoluteSeek 0
  hFindFlac h >>= \case
    Nothing -> throwIO UnknownFormat
    Just flacLoc -> do
      hSeek h AbsoluteSeek 0
      buf <- hGetFileContents h
      case flacLoc of
        0 -> MkFlac <$> bdecodeOrThrowIO buf
        _ -> do
          let (id3buf, flacbuf) = L.splitAt (fromIntegral flacLoc) buf
          id3v23loc <- hLocate @ID3v2_3 h
          hSeek h AbsoluteSeek 0
          id3v24loc <- hLocate @ID3v2_4 h
          hSeek h AbsoluteSeek 0
          metadata <- bdecodeOrThrowIO flacbuf
          if isJust id3v23loc
            then do
              id3 <- bdecodeOrThrowIO id3buf
              pure $ MkFlacWithID3v2_3 id3 metadata
            else
              if isJust id3v24loc
                then do
                  id3 <- bdecodeOrThrowIO id3buf
                  pure $ MkFlacWithID3v2_4 id3 metadata
                else throwIO $ MetadataReadError "ID3v2 detected but couldn't be read"

hWriteFlac :: Handle -> Flac -> IO ()
hWriteFlac h flac' = do
  let buf = L.toStrict $ runPut (putFlac flac')
  BS.hPut h buf
  where
    putFlac (Flac flacMetadata) = put flacMetadata
    putFlac (FlacWithID3v2_3 id3 flacMetadata) = put id3 >> put flacMetadata
    putFlac (FlacWithID3v2_4 id3 flacMetadata) = put id3 >> put flacMetadata

collectFlacMetadata :: Flac -> H.HashMap MetadataId Metadata
collectFlacMetadata (FlacWithID3v2_3 id3v2 f) =
  let id3v2fmt = metadataFormat @ID3v2_3
      vc = vorbisComment f
   in H.fromList $
        catMaybes
          [ Just (id3v2fmt ^. #formatId, extractMetadata id3v2),
            vc <&> (vorbisCommentsId,) . extractMetadata
          ]
collectFlacMetadata (FlacWithID3v2_4 id3v2 f) =
  let id3v2fmt = metadataFormat @ID3v2_4
      vc = vorbisComment f
   in H.fromList $
        catMaybes
          [ Just (id3v2fmt ^. #formatId, extractMetadata id3v2),
            vc <&> (vorbisCommentsId,) . extractMetadata
          ]
collectFlacMetadata (Flac f) =
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
      getInfo :: FlacMetadata -> Info
      getInfo fs =
        let si = streamInfo fs
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

marker :: IsString s => s
marker = "fLaC"

hFindFlac :: Handle -> IO (Maybe Integer)
hFindFlac h = do
  skipId3
  flacLoc <- hTell h
  findFlac flacLoc
  where
    skipId3 = do
      pos <- hTell h
      hLocate @ID3v2_4 h >>= \case
        Just id3v24loc -> findId3End id3v24loc >>= hSeek h AbsoluteSeek
        Nothing -> do
          hSeek h AbsoluteSeek pos
          hLocate @ID3v2_3 h >>= \case
            Just id3v23loc -> findId3End id3v23loc >>= hSeek h AbsoluteSeek
            Nothing -> do
              hSeek h AbsoluteSeek pos
              pure ()
    findId3End loc = do
      hSeek h AbsoluteSeek (fromIntegral loc)
      id3Size <- runGet ID3v2.getId3v2Size <$!> (L.fromStrict <$> BS.hGet h ID3v2.headerSize)
      pure $ fromIntegral loc + id3Size + fromIntegral ID3v2.headerSize
    findFlac flacLoc = do
      hSeek h AbsoluteSeek flacLoc
      buf <- BS.hGet h 4
      pure $
        if buf == marker
          then Just flacLoc
          else Nothing

data FlacMetadata = FlacMetadata
  { streamInfo :: !StreamInfo,
    metadataBlocks :: !(Vector MetadataBlock)
  }
  deriving (Show, Generic)

vorbisComment :: FlacMetadata -> Maybe VorbisComments
vorbisComment (FlacMetadata _ blocks) = findVcs $ V.toList blocks
  where
    findVcs [] = Nothing
    findVcs (m : ms) = case m of
      VorbisCommentBlock _ vcs -> Just vcs
      _otherBlock -> findVcs ms

flacMetadataPictures :: FlacMetadata -> [Picture]
flacMetadataPictures (FlacMetadata _ blocks) = findPictures (V.toList blocks)
  where
    findPictures [] = []
    findPictures (b : bs) = case b of
      PictureBlock _ picture -> picture : findPictures bs
      _otherBlock -> findPictures bs

instance Binary FlacMetadata where
  get = do
    expectGetEq (getByteString 4) marker ("Couldn't find " <> marker <> " marker")
    StreamInfoBlock _ streamInfo <- get
    FlacMetadata streamInfo <$> getMetadataBlocks
  put FlacMetadata {..} =
    putByteString marker
      >> put (StreamInfoBlock False streamInfo)
      >> V.mapM_ put metadataBlocks

getMetadataBlocks :: Get (Vector MetadataBlock)
getMetadataBlocks = V.fromList <$> go
  where
    go = do
      header <- lookAhead get
      block <- get
      if isLast header
        then pure [block]
        else do
          blocks <- go
          pure $ block : blocks

data MetadataBlock
  = StreamInfoBlock !Bool !StreamInfo
  | PaddingBlock !Bool !Padding
  | VorbisCommentBlock !Bool !VorbisComments
  | PictureBlock !Bool !Picture
  | OtherBlock !Bool !Word8 !ByteString
  deriving (Show)

metadataBlockSize :: MetadataBlock -> Integer
metadataBlockSize (StreamInfoBlock _ _) = blockHeaderSize + streamInfoSize
metadataBlockSize (PaddingBlock _ (Padding s)) = blockHeaderSize + toInteger s
metadataBlockSize (OtherBlock _ _ d) = blockHeaderSize + toInteger (BS.length d)
metadataBlockSize (VorbisCommentBlock _ vc) = blockHeaderSize + metadataSize vc
metadataBlockSize (PictureBlock _ p) = blockHeaderSize + pictureSize p

instance Binary MetadataBlock where
  get = do
    header <- get
    case blockType header of
      0 -> StreamInfoBlock (isLast header) <$> get
      1 -> do
        skip (fromIntegral $ blockLength header)
        pure $ PaddingBlock (isLast header) (Padding $ blockLength header)
      4 -> VorbisCommentBlock (isLast header) <$> get
      6 -> PictureBlock (isLast header) <$> get
      127 -> fail "Invalid flac block type 127"
      bt -> do
        let len = blockLength header
        blockData <- getByteString $ fromIntegral len
        pure $ OtherBlock (isLast header) bt blockData
  put (StreamInfoBlock isLast streamInfo) = do
    put
      MetadataBlockHeader
        { blockType = 0,
          blockLength = fromInteger streamInfoSize,
          isLast
        }
    put streamInfo
  put (PaddingBlock isLast (Padding size)) = do
    put
      MetadataBlockHeader
        { blockType = 1,
          blockLength = size,
          isLast = isLast
        }
    replicateM_ (fromIntegral size) $ putWord8 0
  put (VorbisCommentBlock isLast vc) = do
    put
      MetadataBlockHeader
        { blockType = 4,
          blockLength = fromInteger $ metadataSize vc,
          isLast
        }
    put vc
  put (PictureBlock isLast picture) = do
    put
      MetadataBlockHeader
        { blockType = 6,
          blockLength = fromInteger $ pictureSize picture,
          isLast
        }
    put picture
  put (OtherBlock isLast blockType blockData) = do
    put
      MetadataBlockHeader
        { blockType,
          blockLength = fromIntegral $ BS.length blockData,
          isLast
        }
    putByteString blockData

data MetadataBlockHeader = MetadataBlockHeader
  { blockType :: !Word8,
    blockLength :: !Word32,
    isLast :: !Bool
  }
  deriving (Show)

blockHeaderSize :: Integer
blockHeaderSize = 4

instance Binary MetadataBlockHeader where
  get = do
    (isLast, blockType) <-
      BG.runBitGet $ (,) <$> BG.getBool <*> BG.getWord8 7
    blockLength <- get24Bits
    return MetadataBlockHeader {blockType, blockLength, isLast}
  put MetadataBlockHeader {..} =
    BP.runBitPut $ do
      BP.putBool isLast
      BP.putWord8 7 blockType
      BP.putWord32be 24 blockLength

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

streamInfoSize :: Integer
streamInfoSize = 34

instance Binary StreamInfo where
  get = do
    minBlockSize <- getWord16be
    expect (minBlockSize >= 16) ("Invalid min block size " <> show minBlockSize)
    maxBlockSize <- getWord16be
    expect (maxBlockSize >= 16) ("Invalid max block size " <> show maxBlockSize)
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
  put StreamInfo {..} = do
    putWord16be minBlockSize
    putWord16be maxBlockSize
    BP.runBitPut $ do
      BP.putWord32be 24 (fromMaybe 0 minFrameSize)
      BP.putWord32be 24 (fromMaybe 0 maxFrameSize)
      BP.putWord32be 20 sampleRate
      BP.putWord8 3 (channels - 1)
      BP.putWord8 5 (bps - 1)
      BP.putWord64be 36 (fromMaybe 0 samples)
    putByteString md5

newtype Padding = Padding Word32
  deriving (Show)

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

instance From Picture EmbeddedPicture where
  from Picture {..} = EmbeddedPicture {
    mimeType,
    pictureData
  }

instance Binary Picture where
  get = do
    pictureType <- mfilter (<= 20) getWord32be <|> Fail.fail "Invalid picture type"
    mimeType <- getUtf8Text =<< fromIntegral <$> getWord32be
    description <- getUtf8Text =<< fromIntegral <$> getWord32be
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
  put Picture {..} = do
    putWord32be pictureType
    let mimeType' = encodeUtf8 mimeType
    putWord32be $ fromIntegral $ BS.length mimeType'
    putByteString mimeType'
    let description' = encodeUtf8 description
    putWord32be $ fromIntegral $ BS.length description'
    putByteString description'
    putWord32be width
    putWord32be height
    putWord32be depth
    putWord32be (fromMaybe 0 numColours)
    putWord32be $ fromIntegral $ BS.length pictureData
    putByteString pictureData

pictureSize :: Picture -> Integer
pictureSize Picture {..} =
  256
    + toInteger (T.length mimeType + T.length description + BS.length pictureData)

removeID3 :: Flac -> Flac
removeID3 f@(Flac _) = f
removeID3 (FlacWithID3v2_3 _ fs) = Flac fs
removeID3 (FlacWithID3v2_4 _ fs) = Flac fs
