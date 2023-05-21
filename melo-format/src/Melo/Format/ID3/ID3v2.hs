{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Format.ID3.ID3v2
  ( ID3v2 (..),
    newId3v2,
    ID3v2_3,
    id3v23Tag,
    id3v23Id,
    ID3v2_4,
    id3v24Tag,
    id3v24Id,
    ID3v2Version (..),
    headerSize,
    SyncSafe,
    fromSyncSafe,
    toSyncSafe,
    changeVersion,
    getId3v2Size,
    hSkip,
    Picture (..),
    id3v2Pictures,
    setPictures,
    hGetId3v2,
  )
where

import Codec.Compression.Zlib qualified as Zlib
import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Fail as F
import Data.Bifunctor
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as L hiding (split)
import Data.ByteString.Lazy.Search qualified as L
import Data.Coerce
import Data.Foldable as F
import Data.Functor
import Data.List.NonEmpty as NE
import Data.Maybe
import Data.String (IsString)
import Data.Text as T hiding (elem)
import Data.Text.Encoding
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import GHC.Records
import Melo.Format.Error
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Encoding
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import Streaming.Binary qualified as S
import Streaming.ByteString qualified as S
import System.IO
  ( Handle,
    SeekMode (..),
    hSeek,
  )
import Witch

id3v23Tag :: TagMapping -> TagLens
id3v23Tag = mappedTag id3v2_3

id3v24Tag :: TagMapping -> TagLens
id3v24Tag = mappedTag id3v2_4

data ID3v2 (v :: ID3v2Version) = ID3v2
  { headerFlags :: !HeaderFlags,
    paddingSize :: !Word32,
    extendedHeader :: !ByteString,
    frames :: !(Frames v)
  }
  deriving (Generic, Show, Eq)

type ID3v2_3 = ID3v2 'ID3v23

id3v23Id :: MetadataId
id3v23Id = MetadataId "ID3v2_3"

instance MetadataFormat ID3v2_3 where
  metadataFormat =
    MetadataFormat
      { formatId = id3v23Id,
        formatDesc = "ID3v2.3"
      }
  fieldMappingSelector = id3v2_3
  readTags = readId3v2Tags
  replaceWithTags id3 tags = id3 {frames = framesFromTags tags `appendFrames` pictureFrames id3.frames}
  metadataSize = id3v2SizeWithHeader

readId3v2Tags :: ID3v2 v -> Tags
readId3v2Tags id3 =
  let contents = foldlFrames accumFrames [] id3.frames
   in Tags $ V.fromList $ fmap (first toTagKey) contents >>= \(a, bs) -> fmap (a,) bs
  where
    accumFrames acc frame = acc <> catMaybes [extractFrameContent frame]

id3v2SizeWithHeader :: (GetFrame v, PutFrame v) => ID3v2 v -> Integer
id3v2SizeWithHeader ID3v2 {..} =
  toInteger headerSize
    + foldlFrames (\c f -> c + calculateFrameSize headerFlags f) 0 frames
    + toInteger paddingSize

getId3v2Size :: Get Integer
getId3v2Size = fromSyncSafe . totalSize <$!> get

type ID3v2_4 = ID3v2 'ID3v24

id3v24Id :: MetadataId
id3v24Id = MetadataId "ID3v2_4"

instance MetadataFormat ID3v2_4 where
  metadataFormat =
    MetadataFormat
      { formatId = id3v24Id,
        formatDesc = "ID3v2.4"
      }
  fieldMappingSelector = id3v2_4
  readTags = readId3v2Tags
  replaceWithTags id3 tags = id3 {frames = framesFromTags tags `appendFrames` pictureFrames id3.frames}
  metadataSize = id3v2SizeWithHeader

newId3v2 :: FrameEncoding v => Tags -> ID3v2 v
newId3v2 tags =
  ID3v2
    { headerFlags = HeaderFlags 0,
      paddingSize = 128,
      extendedHeader = BS.empty,
      frames = framesFromTags tags
    }

framesFromTags :: FrameEncoding v => Tags -> Frames v
framesFromTags (Tags tags) = case V.toList tags of
  [] -> impureThrow $ MetadataWriteError "ID3v2 must contain at least one frame"
  (t : ts) -> Frames $ createFrame <$> t :| ts

createFrame :: forall (v :: ID3v2Version). FrameEncoding v => (Text, Text) -> Frame v
createFrame (k, v) =
  let frameId = fromTagKey @v k
   in Frame
        { frameId = frameId,
          frameFlags = FrameHeaderFlags 0,
          frameContent = createFrameContent frameId v
        }

changeVersion :: ID3v2 v1 -> ID3v2 v2
changeVersion ID3v2 {..} =
  ID3v2
    { headerFlags,
      paddingSize,
      extendedHeader,
      frames = Frames $ fmap changeFrameVersion (coerce frames)
    }

hGetId3v2 :: (GetFrame v, PutFrame v) => Handle -> IO (Maybe (ID3v2 v))
hGetId3v2 h = do
  -- TODO ID3v2 can also appear at the end of a file
  hSeek h AbsoluteSeek 0
  (_, _, e) <- S.decode (S.hGetContents h)
  case e of
    Left _ -> pure Nothing
    Right id3 -> pure (Just id3)

instance (GetFrame v, PutFrame v) => Binary (ID3v2 v) where
  get = do
    header@Header {..} <- get
    when (version /= id3v2Version @v) $
      fail ("expected ID3v2 version " <> show (id3v2Version @v) <> " got " <> show version)
    extendedHeader <-
      if hasExtendedHeader flags
        then getExtendedHeader
        else pure BS.empty
    frames <- getFrames @v header
    let padding =
          fromSyncSafe totalSize
            - foldlFrames (\c f -> c + calculateFrameSize header.flags f) 0 frames
    pure $ ID3v2 flags (fromInteger padding) extendedHeader frames
    where
      getExtendedHeader = do
        extendedHeaderSize <- fromSyncSafe <$> get
        getByteString (extendedHeaderSize - 4)
  put ID3v2 {..} = do
    let id3data = runPut $ do
          if hasExtendedHeader headerFlags
            then do
              let extendedHeaderSize = BS.length extendedHeader + 4
              put $ toSyncSafe (fromIntegral @_ @Word32 extendedHeaderSize)
              putByteString extendedHeader
            else pure ()
          putFrames headerFlags frames
    let id3v2size = L.length id3data + fromIntegral paddingSize
    let header =
          Header
            { version = id3v2Version @v,
              flags = headerFlags,
              totalSize = toSyncSafe (fromIntegral @_ @Word32 id3v2size)
            }
    put header
    putLazyByteString id3data
    -- TODO id3v2 footer after padding
    replicateM_ (fromIntegral paddingSize) (putWord8 0)

data Header = Header
  { version :: !ID3v2Version,
    flags :: !HeaderFlags,
    totalSize :: !SyncSafe
  }
  deriving (Eq, Show)

headerSize :: Int
headerSize = 10

id3v2Identifier :: IsString s => s
id3v2Identifier = "ID3"

hSkip :: Handle -> IO ()
hSkip h = do
  headerBuf <- BS.hGet h headerSize
  if id3v2Identifier `BS.isPrefixOf` headerBuf
    then do
      case runGetOrFail (get @Header) (L.fromStrict headerBuf) of
        Left _ -> impureThrow $ MetadataReadError "found 'ID3' identifier but failed to read header"
        Right (_, _, header) -> do
          let n = fromSyncSafe header.totalSize
          hSeek h RelativeSeek n
          hSkipZeroes h
          hSkip h
    else hSeek h RelativeSeek (fromIntegral (negate (BS.length headerBuf)))

instance Binary Header where
  get = isolate headerSize $ do
    expectGetEq (getByteString 3) id3v2Identifier "Expected ID3v2 identifier"
    header <- Header <$> get <*> get <*> get
    expect header.flags.isReadable "Unrecognised ID3v2 flags found"
    pure header
  put Header {..} = do
    putByteString id3v2Identifier
    put version
    put flags
    put totalSize

data ID3v2Version = ID3v24 | ID3v23
  deriving (Eq)

instance Show ID3v2Version where
  show = \case
    ID3v24 -> "ID3v2.4"
    ID3v23 -> "ID3v2.3"

instance Binary ID3v2Version where
  get =
    getWord16le >>= \case
      3 -> pure ID3v23
      4 -> pure ID3v24
      a -> F.fail $ "Unknown ID3v2 version " <> show a
  put ID3v24 = putWord16le 4
  put ID3v23 = putWord16le 3

class Version (v :: ID3v2Version) where
  id3v2Version :: ID3v2Version

instance Version 'ID3v24 where
  id3v2Version = ID3v24

instance Version 'ID3v23 where
  id3v2Version = ID3v23

newtype HeaderFlags = HeaderFlags Word8
  deriving (Eq, Show)

instance Binary HeaderFlags where
  get = HeaderFlags <$> getWord8

  put (HeaderFlags f) = putWord8 f

isReadable :: HeaderFlags -> Bool
isReadable (HeaderFlags f) = f .&. 0xF == 0

instance HasField "isReadable" HeaderFlags Bool where
  getField = isReadable

unsynchronisation :: HeaderFlags -> Bool
unsynchronisation (HeaderFlags f) = testBit f 7

instance HasField "unsynchronisation" HeaderFlags Bool where
  getField = unsynchronisation

hasExtendedHeader :: HeaderFlags -> Bool
hasExtendedHeader (HeaderFlags f) = testBit f 6

instance HasField "hasExtendedHeader" HeaderFlags Bool where
  getField = hasExtendedHeader

experimental :: HeaderFlags -> Bool
experimental (HeaderFlags f) = testBit f 5

instance HasField "experimental" HeaderFlags Bool where
  getField = experimental

hasFooter :: HeaderFlags -> Bool
hasFooter (HeaderFlags f) = testBit f 4

instance HasField "hasFooter" HeaderFlags Bool where
  getField = hasFooter

newtype Padding = Padding Word32
  deriving (Eq, Show)

newtype Frames (v :: ID3v2Version) = Frames (NonEmpty (Frame v))
  deriving (Show, Eq)

appendFrames :: Frames v -> [Frame v] -> Frames v
appendFrames (Frames (f :| fs)) newFrames = Frames (f :| fs <> newFrames)

foldlFrames :: (b -> Frame v -> b) -> b -> Frames v -> b
foldlFrames f z (Frames frms) = F.foldl' f z frms

pictureFrames :: Frames v -> [Frame v]
pictureFrames (Frames frames) = NE.filter isPictureFrame frames

id3v2Pictures :: ID3v2 v -> [Picture]
id3v2Pictures id3 = catMaybes $ pictureFrames id3.frames <&> \case
  Frame {frameContent = PictureFrame _ pic} -> Just pic
  _ -> Nothing

isPictureFrame :: Frame v -> Bool
isPictureFrame Frame {frameContent = PictureFrame _ _} = True
isPictureFrame _ = False

mkPictureFrame :: PictureType -> EmbeddedPicture -> Picture
mkPictureFrame pictureType pic = Picture {
    pictureType = fromIntegral $ fromEnum pictureType,
    mimeType = pic.mimeType,
    description = "",
    pictureData = pic.pictureData
  }

setPictures :: forall v. FrameEncoding v => [(PictureType, EmbeddedPicture)] -> ID3v2 v -> ID3v2 v
setPictures pics id3 =
  let frames = NE.filter (not . isPictureFrame) (coerce id3.frames)
      picContents = uncurry mkPictureFrame <$> pics
      frameId = PreDefinedId "APIC"
      enc = frameContentEncoding @v frameId
      picFrames = Frame frameId (FrameHeaderFlags 0) . PictureFrame enc <$> picContents in
  id3 { frames = Frames $ NE.fromList (frames <> picFrames) }

type GetFrame v =
  ( GetFrameHeader v,
    GetEncoding v,
    Version v,
    FrameCompressed v,
    FrameDataLength v,
    FrameUnsynchronisation v
  )

getFrames :: GetFrame v => Header -> Get (Frames v)
getFrames Header {flags, totalSize} = Frames <$> getFrames'
  where
    getFrames' = liftM2 (:|) (getFrame flags) getRest
    getRest =
      isEnd >>= \case
        True -> pure []
        False -> liftM2 (:) (getFrame flags) getRest
    endPos = fromIntegral $ fromSyncSafe totalSize + headerSize
    isEnd =
      bytesRead >>= \case
        n | n == endPos -> pure True
        _ ->
          lookAhead (getByteString 1)
            <&> if hasFooter flags
              then \case
                -- found footer
                "3" -> True
                _ -> False
              else \case
                -- found padding
                "\0" -> True
                _ -> False

type PutFrame v = (PutFrameHeader v, PutEncoding v, PutTextContent v, Version v)

putFrames :: (GetFrame v, PutFrame v) => HeaderFlags -> Frames v -> Put
putFrames flags (Frames frames) = mapM_ (putFrame flags) frames

data Frame (v :: ID3v2Version) = Frame
  { frameId :: !FrameId,
    frameFlags :: !FrameHeaderFlags,
    frameContent :: !(FrameContent v)
  }
  deriving (Eq, Show)

getFrame :: forall v. GetFrame v => HeaderFlags -> Get (Frame v)
getFrame flags = do
  (header, frameData) <- do
    header <- getFrameHeader
    frameData <- getFrameData header
    let header' = header {frameSize = fromIntegral (L.length frameData)}
    pure (header', frameData)
  case runGetOrFail (getFrameImpl header) frameData of
    Left (_, _, e) -> fail e
    Right (_, _, frame) -> pure frame
  where
    getFrameImpl header | isText header = do
      enc <- getEncoding @v
      frameId' <- getFrameId @v header enc
      let enc' = if "W" `T.isPrefixOf` header.frameId then NullTerminated else enc
      Frame frameId' header.frameFlags . mkFrameContent frameId' enc' <$> getTextContent header frameId' enc'
    getFrameImpl header | isPicture header = do
      enc <- getEncoding @v
      picture <- getPicture @v header enc
      let frameId' = PreDefinedId header.frameId
      pure $ Frame frameId' header.frameFlags (PictureFrame enc picture)
    getFrameImpl header = do
      a <- bytesRead
      frameId' <- getFrameId @v header NullTerminated
      b <- bytesRead
      frameContent <- OtherFrame <$> getByteString (fromIntegral header.frameSize - fromIntegral (b - a))
      pure $ Frame frameId' header.frameFlags frameContent
    isText :: FrameHeader v -> Bool
    isText FrameHeader {frameId} = case T.uncons frameId of
      Just ('T', _) -> True
      Just ('W', _) -> True
      _ -> False
    isPicture :: FrameHeader v -> Bool
    isPicture header = header.frameId == "APIC"
    getFrameData :: FrameHeader v -> Get L.ByteString
    getFrameData header = do
      dataLength <-
        if frameHasDataLength @v header.frameFlags || frameIsCompressed @v header.frameFlags
          then Just <$> getFrameDataLength @v
          else pure Nothing

      frameData <-
        if frameHasUnsynchronisation @v flags header.frameFlags
          then do
            frameData <- getLazyByteString (fromIntegral header.frameSize - if isJust dataLength then 4 else 0)
            let synced = L.replace "\xff\x00" ("\xff" :: ByteString) frameData
            unless (isNothing dataLength || fmap fromIntegral dataLength == Just (L.length synced)) do
              fail $ "synced data length mismatch. expected " <> show dataLength <> " got " <> show (L.length synced)
            pure synced
          else getLazyByteString (fromIntegral header.frameSize)
      if frameIsCompressed @v header.frameFlags
        then do
          let decompressed = Zlib.decompress frameData
          unless (isNothing dataLength || fmap fromIntegral dataLength == Just (L.length decompressed)) do
            fail $ "decompressed data length mismatch. expected " <> show dataLength <> " got " <> show (L.length decompressed)
          pure decompressed
        else pure frameData

putFrame :: forall v. (GetFrame v, PutFrame v) => HeaderFlags -> Frame v -> Put
putFrame flags Frame {..} = do
  let fid = case frameId of
        PreDefinedId fid' -> fid'
        UserDefinedId fid' _enc _ -> fid'
  let frameContentData = runPut $ putFrameContent frameId frameContent

  (frameContentData', frameFlags) <-
    if frameIsCompressed @v frameFlags
      then pure (Zlib.compress frameContentData, setFrameHasDataLength @v frameFlags True)
      else pure (frameContentData, frameFlags)
  (frameContentData', frameFlags) <-
    if frameHasUnsynchronisation @v flags frameFlags
      then pure (unsync frameContentData', frameFlags)
      else pure (frameContentData', frameFlags)
  let shouldWriteDataLengthInd = frameHasDataLength @v frameFlags || frameIsCompressed @v frameFlags
  let frameSize = fromIntegral $ L.length frameContentData' + if shouldWriteDataLengthInd then 4 else 0
  let header :: FrameHeader v =
        FrameHeader
          { frameId = fid,
            frameSize,
            frameFlags
          }
  putFrameHeader header
  when shouldWriteDataLengthInd $
    putFrameDataLength @v $
      fromIntegral (L.length frameContentData)
  putLazyByteString frameContentData'
  where
    unsync d = go (L.splitKeepFront "\xFF" d)
    go (c : cs) = case L.unpack c of
      (0xFF : 0x00 : ws) -> L.pack (0xFF : 0x00 : 0x00 : ws) <> go cs
      (0xFF : w : ws) | w .&. 0xE0 == 0xE0 -> "\xFF\x00" <> go (L.pack (w : ws) : cs)
      (0xFF : []) -> case cs of
        (c : cs) -> go (L.cons 0xFF c : cs)
        cs -> "\xFF" <> go cs
      (_ : _) -> c <> go cs
      [] -> go cs
    go [] = L.empty

calculateFrameSize :: (GetFrame v, PutFrame v) => HeaderFlags -> Frame v -> Integer
calculateFrameSize flags = fromIntegral . L.length . runPut . putFrame flags

changeFrameVersion :: Frame v1 -> Frame v2
changeFrameVersion Frame {..} =
  Frame
    { frameId,
      frameFlags,
      frameContent = changeContentVersion frameContent
    }

data FrameId
  = PreDefinedId !Text
  | UserDefinedId !Text !TextEncoding !Text
  deriving (Show, Eq)

toTagKey :: FrameId -> Text
toTagKey (PreDefinedId fid) = fid
toTagKey (UserDefinedId fid1 _enc fid2) = fid1 <> ";" <> fid2

class FrameEncoding (v :: ID3v2Version) where
  frameIdEncoding :: TextEncoding
  frameContentEncoding :: FrameId -> TextEncoding

instance FrameEncoding 'ID3v23 where
  frameIdEncoding = UCS2
  frameContentEncoding fid = case fid of
    PreDefinedId "TYER" -> NullTerminated
    PreDefinedId "TRCK" -> NullTerminated
    PreDefinedId fid | "W" `T.isPrefixOf` fid -> NullTerminated
    UserDefinedId fid _ _ | "W" `T.isPrefixOf` fid -> NullTerminated
    _ -> UCS2

instance FrameEncoding 'ID3v24 where
  frameIdEncoding = UTF8
  frameContentEncoding fid = case fid of
    PreDefinedId "TDRC" -> NullTerminated
    PreDefinedId "TRCK" -> NullTerminated
    PreDefinedId fid | "W" `T.isPrefixOf` fid -> NullTerminated
    UserDefinedId fid _ _ | "W" `T.isPrefixOf` fid -> NullTerminated
    _ -> UTF8

fromTagKey :: forall (v :: ID3v2Version). FrameEncoding v => Text -> FrameId
fromTagKey k = case T.span (/= ';') k of
  (fid, "") -> PreDefinedId fid
  (fid, desc) -> UserDefinedId fid (frameIdEncoding @v) (T.drop 1 desc)

getFrameId :: forall (v :: ID3v2Version). FrameHeader v -> TextEncoding -> Get FrameId
getFrameId header@FrameHeader {frameId} enc = case T.uncons frameId of
  Just ('U', "FID") -> UserDefinedId frameId NullTerminated <$> getNullTerminatedAscii
  Just (_, "XXX") -> UserDefinedId frameId enc <$> getUserDefinedFrameId header enc
  _ -> pure $ PreDefinedId frameId

getUserDefinedFrameId :: FrameHeader v -> TextEncoding -> Get Text
getUserDefinedFrameId header enc = do
  let sz = frameSize header
  bs <- lookAhead $ getByteString $ fromIntegral (sz - 1)
  let !uid = case (enc, fst (BS.breakSubstring (terminator enc) bs)) of
        (enc, uid') | enc `elem` [UTF16, UCS2] && BS.length uid' `mod` 2 /= 0 -> uid' <> "\0"
        (_, uid') -> uid'
  skip $ BS.length uid + BS.length (terminator enc)
  decodeID3Text enc uid

data FrameHeader (v :: ID3v2Version) = FrameHeader
  { frameId :: !Text,
    frameSize :: !Word32,
    frameFlags :: !FrameHeaderFlags
  }
  deriving (Show, Eq)

class GetFrameHeader (v :: ID3v2Version) where
  getFrameHeader :: Get (FrameHeader v)

instance GetFrameHeader 'ID3v23 where
  getFrameHeader = FrameHeader <$> getUtf8Text 4 <*> getWord32be <*> get

instance GetFrameHeader 'ID3v24 where
  getFrameHeader = FrameHeader <$> getUtf8Text 4 <*> (fromSyncSafe <$> get) <*> get

class PutFrameHeader (v :: ID3v2Version) where
  putFrameHeader :: FrameHeader v -> Put

instance PutFrameHeader 'ID3v23 where
  putFrameHeader FrameHeader {..} = do
    putUtf8Text (T.take 4 frameId)
    putWord32be frameSize
    put frameFlags

instance PutFrameHeader 'ID3v24 where
  putFrameHeader FrameHeader {..} = do
    putUtf8Text (T.take 4 frameId)
    put (toSyncSafe frameSize)
    put frameFlags

newtype FrameHeaderFlags = FrameHeaderFlags Word16
  deriving (Eq, Show)

instance Binary FrameHeaderFlags where
  get = FrameHeaderFlags <$> getWord16be
  put (FrameHeaderFlags f) = putWord16be f

class FrameCompressed (v :: ID3v2Version) where
  frameIsCompressed :: FrameHeaderFlags -> Bool
  setFrameCompressed :: FrameHeaderFlags -> Bool -> FrameHeaderFlags

instance FrameCompressed 'ID3v23 where
  frameIsCompressed (FrameHeaderFlags flags) = testBit flags 7
  setFrameCompressed (FrameHeaderFlags flags) b = FrameHeaderFlags if b then setBit flags 7 else clearBit flags 7

instance FrameCompressed 'ID3v24 where
  frameIsCompressed (FrameHeaderFlags flags) = testBit flags 3
  setFrameCompressed (FrameHeaderFlags flags) b = FrameHeaderFlags if b then setBit flags 3 else clearBit flags 3

class FrameUnsynchronisation (v :: ID3v2Version) where
  frameHasUnsynchronisation :: HeaderFlags -> FrameHeaderFlags -> Bool

instance FrameUnsynchronisation 'ID3v23 where
  frameHasUnsynchronisation flags = const (unsynchronisation flags)

instance FrameUnsynchronisation 'ID3v24 where
  frameHasUnsynchronisation flags (FrameHeaderFlags frameFlags) = unsynchronisation flags || testBit frameFlags 1

class FrameDataLength (v :: ID3v2Version) where
  frameHasDataLength :: FrameHeaderFlags -> Bool
  setFrameHasDataLength :: FrameHeaderFlags -> Bool -> FrameHeaderFlags
  getFrameDataLength :: Get Word32
  putFrameDataLength :: Word32 -> Put

instance FrameDataLength 'ID3v23 where
  frameHasDataLength = frameIsCompressed @'ID3v23
  setFrameHasDataLength f _ = f
  getFrameDataLength = getWord32be
  putFrameDataLength = putWord32be

instance FrameDataLength 'ID3v24 where
  frameHasDataLength (FrameHeaderFlags flags) = testBit flags 0
  setFrameHasDataLength (FrameHeaderFlags flags) b = FrameHeaderFlags if b then setBit flags 0 else clearBit flags 0
  getFrameDataLength = fromSyncSafe <$> get
  putFrameDataLength = put . toSyncSafe

data FrameContent (v :: ID3v2Version)
  = TextFrame !TextEncoding ![Text]
  | UrlFrame !TextEncoding ![Text]
  | PictureFrame !TextEncoding !Picture
  | OtherFrame !ByteString
  deriving (Show, Eq)

mkFrameContent :: FrameId -> TextEncoding -> [Text] -> FrameContent v
mkFrameContent fid enc t = case fid of
  PreDefinedId i -> cid i
  UserDefinedId i _enc _ -> cid i
  where
    cid = \case
      i | T.isPrefixOf "T" i -> TextFrame enc t
      i | T.isPrefixOf "W" i -> UrlFrame enc t
      _otherFrame -> OtherFrame BS.empty

putFrameContent :: forall v. PutFrame v => FrameId -> FrameContent v -> Put
putFrameContent f (TextFrame enc content) = putTextContent @v f enc content
putFrameContent f (UrlFrame enc content) = putTextContent @v f enc content
putFrameContent _ (PictureFrame enc picture) = putPicture @v enc picture
putFrameContent (UserDefinedId "UFID" _ owner) (OtherFrame content) = putByteString (encodeLatin1 owner) >> putWord8 0 >> putByteString content
putFrameContent _ (OtherFrame content) = putByteString content

extractFrameContent :: Frame v -> Maybe (FrameId, [Text])
extractFrameContent Frame {frameId, frameContent} = case frameContent of
  TextFrame _ vs -> Just (frameId, vs)
  UrlFrame _ vs -> Just (frameId, vs)
  OtherFrame bs -> case frameId of
    UserDefinedId "UFID" _ _owner -> Just (frameId, [decodeLatin1 bs])
    _ -> Nothing
  _otherFrame -> Nothing

createFrameContent :: forall v. FrameEncoding v => FrameId -> Text -> FrameContent v
createFrameContent frameId val = case T.uncons (toTagKey frameId) of
  Just ('T', _) -> TextFrame (frameContentEncoding @v frameId) [val]
  Just ('W', _) -> UrlFrame (frameContentEncoding @v frameId) [val]
  Just _ -> OtherFrame (encodeLatin1 val)
  _ -> impureThrow $ MetadataWriteError ("invalid ID3v2 text frame id " <> T.pack (show frameId))

changeContentVersion :: FrameContent v1 -> FrameContent v2
changeContentVersion (TextFrame enc content) = TextFrame enc content
changeContentVersion (UrlFrame enc content) = UrlFrame enc content
changeContentVersion (PictureFrame enc picture) = PictureFrame enc picture
changeContentVersion (OtherFrame content) = OtherFrame content

getTextContent :: forall (v :: ID3v2Version). FrameHeader v -> FrameId -> TextEncoding -> Get [Text]
getTextContent FrameHeader {frameSize} frameId' enc = do
  let fidSz = case frameId' of
        UserDefinedId _ frameIdEnc t -> fromIntegral $ BS.length (encodeID3Text frameIdEnc t)
        _otherFrameId -> 0
  let sz = frameSize - 1 - fidSz
  bs <- getByteString (fromIntegral sz)
  mapM (decodeID3Text enc) (splitFields (terminator enc) bs)

splitFields :: ByteString -> ByteString -> [ByteString]
splitFields term fields
  | BS.null fields = []
  | otherwise =
      h
        : if BS.null t then [] else splitFields term (BS.drop (BS.length term) t)
  where
    (h, t) = BS.breakSubstring term (fromMaybe fields $ BS.stripSuffix term fields)

class PutTextContent (v :: ID3v2Version) where
  putTextContent :: FrameId -> TextEncoding -> [Text] -> Put

instance PutFrame v => PutTextContent v where
  putTextContent frameId enc content = do
    case frameId of
      UserDefinedId _ enc desc -> do
        putEncoding @v enc
        putByteString (encodeID3Text enc desc)
      _otherFrameId -> putEncoding @v enc
    putByteString encodedText
    where
      encodedText = BS.concat (fmap (encodeID3Text enc) content)

data TextEncoding = NullTerminated | UCS2 | UTF16 | UTF16BE | UTF8
  deriving (Show, Eq)

terminator :: TextEncoding -> ByteString
terminator NullTerminated = "\0"
terminator UCS2 = "\0\0"
terminator UTF8 = "\0"
terminator UTF16 = "\0\0"
terminator UTF16BE = "\0\0"

decodeID3Text :: MonadFail m => TextEncoding -> ByteString -> m Text
decodeID3Text NullTerminated bs = pure $ decodeLatin1 bs
decodeID3Text UTF8 bs = decodeUtf8OrFail bs
decodeID3Text UTF16BE bs = decodeUtf16BEOrFail bs
decodeID3Text UTF16 bs = decodeUtf16WithBOMOrFail bs
decodeID3Text UCS2 bs = decodeUtf16WithBOMOrFail bs

encodeID3Text :: TextEncoding -> Text -> ByteString
encodeID3Text enc t = encodedText <> terminator enc
  where
    encodedText = case enc of
      NullTerminated -> encodeLatin1 t
      UTF8 -> encodeUtf8 t
      UTF16BE -> encodeUtf16BE t
      UTF16 -> utf16LeBom <> encodeUtf16LE t
      UCS2 -> utf16LeBom <> encodeUtf16LE t

class GetEncoding (v :: ID3v2Version) where
  getEncoding :: Get TextEncoding

instance GetEncoding 'ID3v23 where
  getEncoding =
    getWord8 >>= \case
      0 -> pure NullTerminated
      1 -> pure UCS2
      x -> F.fail $ "Unrecognised ID3v2.3 encoding flag " <> show x

instance GetEncoding 'ID3v24 where
  getEncoding =
    getWord8 >>= \case
      0 -> pure NullTerminated
      1 -> pure UTF16
      2 -> pure UTF16BE
      3 -> pure UTF8
      x -> F.fail $ "Unrecognised ID3v2.4 encoding flag " <> show x

class PutEncoding (v :: ID3v2Version) where
  putEncoding :: TextEncoding -> Put

instance PutEncoding 'ID3v23 where
  putEncoding = \case
    NullTerminated -> putWord8 0
    UCS2 -> putWord8 1
    x ->
      impureThrow $
        MetadataWriteError $
          "Unsupported ID3v2.3 encoding flag " <> T.pack (show x)

instance PutEncoding 'ID3v24 where
  putEncoding = \case
    NullTerminated -> putWord8 0
    UTF16 -> putWord8 1
    UTF16BE -> putWord8 2
    UTF8 -> putWord8 3
    x ->
      impureThrow $
        MetadataWriteError $
          "Unsupported ID3v2.4 encoding flag " <> T.pack (show x)

data Picture = Picture
  { pictureType :: !Word8,
    mimeType :: !Text,
    description :: !Text,
    pictureData :: !ByteString
  }
  deriving (Show, Eq)

instance From Picture EmbeddedPicture where
  from Picture {..} =
    EmbeddedPicture
      { mimeType,
        pictureData
      }

getPicture :: forall (v :: ID3v2Version). FrameHeader v -> TextEncoding -> Get Picture
getPicture header enc = do
  mimeType <- getNullTerminatedAscii
  pictureType <- mfilter (<= 20) getWord8 <|> fail "Invalid picture type"
  let sz = fromIntegral header.frameSize - (T.length mimeType + 1) - 2 {- enc + pictureType -}
  buf <- getByteString sz
  let term = terminator enc
  let (description, pictureData) = BS.breakSubstring term buf
  description' <- decodeID3Text enc description
  pure $
    Picture
      { pictureType,
        mimeType,
        description = description',
        pictureData = BS.drop (BS.length term) pictureData
      }

putPicture :: forall (v :: ID3v2Version). PutEncoding v => TextEncoding -> Picture -> Put
putPicture enc picture = do
  putEncoding @v enc
  putByteString (encodeLatin1 picture.mimeType)
  putWord8 0
  putWord8 picture.pictureType
  putByteString $ encodeID3Text enc picture.description
  putByteString picture.pictureData

newtype SyncSafe = SyncSafe
  { syncSafe :: ByteString
  }
  deriving (Eq, Show)

fromSyncSafe :: forall a. (Num a, Bits a) => SyncSafe -> a
fromSyncSafe (SyncSafe s) =
  let s' = fromIntegral <$> BS.unpack s
   in go s' (BS.length s - 1)
  where
    go :: [a] -> Int -> a
    go [] _ = 0
    go (x : xs) i = shiftL (x .&. 0x7F) (7 * i) .|. go xs (i - 1)

toSyncSafe :: (Integral a, Bits a) => a -> SyncSafe
toSyncSafe a =
  SyncSafe $
    BS.singleton (fromIntegral (shiftR a 21 .&. 0x7F))
      `BS.snoc` fromIntegral (shiftR a 14 .&. 0x7F)
      `BS.snoc` fromIntegral (shiftR a 7 .&. 0x7F)
      `BS.snoc` fromIntegral (a .&. 0x7F)

isSyncSafe :: ByteString -> Bool
isSyncSafe bs = not $ BS.any (`testBit` 7) bs

instance Binary SyncSafe where
  get = do
    expectGet_ (lookAhead $ getByteString 4) isSyncSafe "Invalid sync safe integer"
    SyncSafe <$> getByteString 4

  put (SyncSafe s) = putByteString (BS.take 4 s)
