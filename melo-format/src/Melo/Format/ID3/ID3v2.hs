{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Format.ID3.ID3v2
  ( ID3v2(..),
    ID3v2_3,
    id3v23Tag,
    id3v23Id,
    ID3v2_4,
    id3v24Tag,
    id3v24Id,
    headerSize,
    SyncSafe,
    fromSyncSafe,
    toSyncSafe,
  )
where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Fail as F
import Data.Bifunctor
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Foldable as F
import Data.Functor
import Data.List.NonEmpty
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import GHC.Generics
import Lens.Micro
import Melo.Format.Error
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Encoding
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import System.IO
  ( SeekMode
      ( AbsoluteSeek
      ),
    hSeek,
  )

id3v23Tag :: TagMapping -> TagLens
id3v23Tag = mappedTag id3v2_3

id3v24Tag :: TagMapping -> TagLens
id3v24Tag = mappedTag id3v2_4

data ID3v2 (v :: ID3v2Version) = ID3v2 {
  headerFlags :: !HeaderFlags,
  id3v2size :: !Integer,
  extendedHeader :: !ByteString,
  frames :: !(Frames v)
} deriving (Generic, Show)

type ID3v2_3 = ID3v2 'ID3v23

id3v23Id :: MetadataId
id3v23Id = MetadataId "ID3v2_3"

instance MetadataFormat ID3v2_3 where
  metadataFormat =
    MetadataFormat
      { formatId = id3v23Id,
        formatDesc = "ID3v2.3"
      }
  metadataLens = id3v23Tag

type ID3v2_4 = ID3v2 'ID3v24

id3v24Id :: MetadataId
id3v24Id = MetadataId "ID3v2_4"

instance MetadataFormat ID3v2_4 where
  metadataFormat =
    MetadataFormat
      { formatId = id3v24Id,
        formatDesc = "ID3v2.4"
      }
  metadataLens = id3v24Tag

instance (GetFrame v, PutFrame v) => MetadataLocator (ID3v2 v) where
  locate bs =
    case runGetOrFail get bs of
      Left _ -> Nothing
      Right (_, _, header) -> case version header of
        version | version == id3v2Version @v -> pure 0
        _ -> Nothing

  hLocate h = do
    hSeek h AbsoluteSeek 0
    bs <- hGetFileContents h
    pure $ fromIntegral <$> locate @(ID3v2 v) bs

instance (GetFrame v, PutFrame v) => Binary (ID3v2 v) where
  get = do
    header <- get
    extendedHeader <- if hasExtendedHeader (flags header)
      then
        getExtendedHeader
      else
        pure BS.empty
    ID3v2 (flags header) (fromSyncSafe $ totalSize header) extendedHeader
      <$> getFrames @v header
    where
      getExtendedHeader = do
        headerSize <- fromSyncSafe <$> get
        getByteString headerSize
  put ID3v2 {..} = do
    -- TODO recalculate size - recalculate padding
    put $ Header {
      version = id3v2Version @v,
      flags = headerFlags,
      totalSize = toSyncSafe (fromIntegral @_ @Word32 id3v2size)
    }
    if hasExtendedHeader headerFlags
      then do
        put $ toSyncSafe (BS.length extendedHeader)
        put extendedHeader
      else pure ()
    putFrames frames
    -- TODO id3v2 padding
    -- TODO id3v2 footer
    pure ()

instance TagReader (ID3v2 v) where
  readTags id3 =
    let frames' = id3 ^. #frames
        contents = foldlFrames accumFrames [] frames'
     in Tags $ V.fromList $ fmap (first toTagKey) contents >>= \(a, bs) -> fmap (a,) bs
    where
      accumFrames acc frame = acc <> catMaybes [extractFrameContent frame]

instance MetadataSize (ID3v2 v) where
  metadataSize = id3v2size

data Header = Header
  { version :: !ID3v2Version,
    flags :: !HeaderFlags,
    totalSize :: !SyncSafe
  }
  deriving (Eq, Show)

headerSize :: Int
headerSize = 10

instance Binary Header where
  get = isolate headerSize $ do
    expectGetEq (getByteString 3) "ID3" "Expected ID3v2 identifier"
    header <- Header <$> get <*> get <*> get
    expect (isReadable (flags header)) "Unrecognised ID3v2 flags found"
    pure header
  put Header {..} = do
    putByteString "ID3"
    put version
    put flags
    put totalSize

newtype Footer = Footer Header

--instance Binary Footer where
--  get = do
--    expectGetEq (getByteString 3) "3DI" "Expected ID3v2 identifier"
--    footer <- Header <$> get <*> get <*> get
--    expect (isReadable (flags footer)) "Unrecognised ID3v2 flags found"
--    pure $ Footer footer

data ID3v2Version = ID3v24 | ID3v23
  deriving (Eq)

instance Show ID3v2Version where
  show = \case
    ID3v24 -> "ID3v2.4"
    ID3v23 -> "ID3v2.3"

instance Binary ID3v2Version where
  get = getWord16le >>= \case
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

unsynchronisation :: HeaderFlags -> Bool
unsynchronisation (HeaderFlags f) = testBit f 7

hasExtendedHeader :: HeaderFlags -> Bool
hasExtendedHeader (HeaderFlags f) = testBit f 6

experimental :: HeaderFlags -> Bool
experimental (HeaderFlags f) = testBit f 5

hasFooter :: HeaderFlags -> Bool
hasFooter (HeaderFlags f) = testBit f 4

newtype Padding = Padding Word32
  deriving (Eq, Show)

newtype Frames (v :: ID3v2Version) = Frames (NonEmpty (Frame v))
  deriving (Show)

foldlFrames :: (b -> Frame v -> b) -> b -> Frames v -> b
foldlFrames f z (Frames frms) = F.foldl f z frms

type GetFrame v = (GetFrameHeader v, GetEncoding v, GetFrameId v, GetTextContent v, Version v)

getFrames :: (GetFrame v, PutFrame v) => Header -> Get (Frames v)
getFrames header = Frames <$> getFrames'
  where
    flags' = flags header
    getFrames' = liftM2 (:|) get getRest
    getRest = isEnd >>= \case
      True -> pure []
      False -> liftM2 (:) get getRest
    isEnd =
      lookAhead (getByteString 1)
        <&> if hasFooter flags'
          then \case
            -- found footer
            "3" -> True
            _ -> False
          else \case
            -- found padding
            "\0" -> True
            _ -> False

type PutFrame v = (PutFrameHeader v, PutEncoding v, PutFrameId v, PutTextContent v, Version v)

putFrames :: (GetFrame v, PutFrame v) => Frames v -> Put
putFrames (Frames frames) = mapM_ put frames

data Frame (v :: ID3v2Version) = Frame {
  frameId :: FrameId,
  frameFlags :: FrameHeaderFlags,
  frameContent :: FrameContent v
} deriving (Eq, Show)

instance (GetFrame v, PutFrame v) => Binary (Frame v) where
  get = do
    header@FrameHeader{..} <- get @(FrameHeader v)
    if isText header then do
        enc <- getEncoding @v
        frameId <- getFrameId @v header enc
        Frame frameId frameFlags . mkFrameContent frameId enc <$> getTextContent header frameId enc
    else do
      frameId <- getFrameId @v header NullTerminated
      frameContent <- OtherFrame <$> getByteString (fromIntegral frameSize)
      pure $ Frame frameId frameFlags frameContent
    where
      isText :: FrameHeader v -> Bool
      isText FrameHeader{frameId} = T.isPrefixOf "T" frameId || T.isPrefixOf "W" frameId

  put Frame {..} = do
    let fid = case frameId of
          PreDefinedId fid -> fid
          UserDefinedId fid _ -> fid
    let frameContentData = runPut $ putFrameContent frameContent
    let frameSize = fromIntegral $ L.length frameContentData
    let header :: FrameHeader v = FrameHeader {
      frameId = fid,
      frameSize,
      frameFlags
    }
    put header
    -- TODO user defined frame ids
    putLazyByteString frameContentData

data FrameId
  = PreDefinedId !Text
  | UserDefinedId !Text !Text
  deriving (Show, Eq)

toTagKey :: FrameId -> Text
toTagKey (PreDefinedId fid) = fid
toTagKey (UserDefinedId fid1 fid2) = fid1 <> ";" <> fid2

class GetFrameId (v :: ID3v2Version) where
  getFrameId :: FrameHeader v -> TextEncoding -> Get FrameId

instance GetFrameId v where
  getFrameId header@FrameHeader{frameId} enc =
    if T.isSuffixOf "XXX" frameId
      then UserDefinedId frameId <$> getUserDefinedFrameId header enc
      else pure $ PreDefinedId frameId

getUserDefinedFrameId :: FrameHeader v -> TextEncoding -> Get Text
getUserDefinedFrameId header enc = do
  let sz = frameSize header
  bs <- lookAhead $ getByteString $ fromIntegral sz
  let uid = fst (BS.breakSubstring (terminator enc) bs)
  skip $ BS.length uid
  decodeID3Text enc uid

class PutFrameId (v :: ID3v2Version) where
  putFrameId :: FrameId -> Put

instance PutFrameId v where
  putFrameId (PreDefinedId fid) = putUtf8Text (T.take 4 fid)

data FrameHeader (v :: ID3v2Version) = FrameHeader
  { frameId :: !Text,
    frameSize :: !Word32,
    frameFlags :: !FrameHeaderFlags
  }

deriving instance Eq (FrameHeader v)

deriving instance Show (FrameHeader v)

instance (GetFrameHeader v, PutFrameHeader v) => Binary (FrameHeader v) where
  get = getFrameHeader
  put = putFrameHeader

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
    putUtf8Text frameId
    put (toSyncSafe frameSize)
    put frameFlags

newtype FrameHeaderFlags = FrameHeaderFlags Word16
  deriving (Eq, Show)

instance Binary FrameHeaderFlags where
  get = FrameHeaderFlags <$> getWord16be
  put (FrameHeaderFlags f) = putWord16be f

data FrameContent (v :: ID3v2Version)
  = TextFrame TextEncoding ![Text]
  | UrlFrame TextEncoding ![Text]
  | OtherFrame !ByteString
  deriving (Show, Eq)

mkFrameContent :: FrameId -> TextEncoding -> [Text] -> FrameContent v
mkFrameContent fid enc t = case fid of
  PreDefinedId i -> cid i
  UserDefinedId i _ -> cid i
  where
    cid i = case i of
      i | T.isPrefixOf "T" i -> TextFrame enc t
      i | T.isPrefixOf "W" i -> UrlFrame enc t
      _ -> OtherFrame BS.empty

putFrameContent :: forall v. PutFrame v => FrameContent v -> Put
putFrameContent (TextFrame enc content) = putTextContent @v enc content
putFrameContent (UrlFrame enc content) = putTextContent @v enc content
putFrameContent (OtherFrame content) = putByteString content

extractFrameContent :: Frame v -> Maybe (FrameId, [Text])
extractFrameContent Frame {frameId, frameContent} = case frameContent of
  TextFrame _ vs -> Just (frameId, vs)
  UrlFrame _ vs -> Just (frameId, vs)
  _ -> Nothing

class GetTextContent (v :: ID3v2Version) where
  getTextContent :: FrameHeader v -> FrameId -> TextEncoding -> Get [Text]

instance GetTextContent v where
  getTextContent FrameHeader {frameId, frameSize} frameId' enc = do
    let fidSz = case frameId' of
          UserDefinedId _ t -> fromIntegral $ T.length t
          _ -> 0
    let sz = frameSize - 1 - fidSz
    bs <- BS.dropWhile (== 0) <$> getByteString (fromIntegral sz)
    let enc' = if "W" `T.isPrefixOf` frameId then NullTerminated else enc
    mapM (decodeID3Text enc') (splitFields (terminator enc') bs)

splitFields :: ByteString -> ByteString -> [ByteString]
splitFields term fields
  | BS.null fields = []
  | otherwise =
    h
      : if BS.null t then [] else splitFields term (BS.drop (BS.length term) t)
  where
    (h, t) = BS.breakSubstring term (fromMaybe fields $ BS.stripSuffix term fields)

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

class PutTextContent (v :: ID3v2Version) where
  putTextContent :: TextEncoding -> [Text] -> Put

instance PutFrame v => PutTextContent v where
  putTextContent enc content = do
    putEncoding @v enc
    let contentBytes = BS.intercalate (terminator enc) (fmap encodeUtf8 content)
    putByteString contentBytes

data TextEncoding = NullTerminated | UCS2 | UTF16 | UTF16BE | UTF8
  deriving (Show, Eq)

class GetEncoding (v :: ID3v2Version) where
  getEncoding :: Get TextEncoding

instance GetEncoding 'ID3v23 where
  getEncoding = getWord8 >>= \case
    0 -> pure NullTerminated
    1 -> pure UCS2
    x -> F.fail $ "Unrecognised ID3v2.3 encoding flag " <> show x

instance GetEncoding 'ID3v24 where
  getEncoding = getWord8 >>= \case
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
    x -> impureThrow $
      MetadataWriteError $ "Unsupported ID3v2.3 encoding flag " <> T.pack (show x)

instance PutEncoding 'ID3v24 where
  putEncoding = \case
    NullTerminated -> putWord8 0
    UTF16 -> putWord8 1
    UTF16BE -> putWord8 1
    UTF8 -> putWord8 3
    x -> impureThrow $
      MetadataWriteError $ "Unsupported ID3v2.4 encoding flag " <> T.pack (show x)

newtype SyncSafe = SyncSafe
  { syncSafe :: ByteString
  }
  deriving (Eq, Show)

fromSyncSafe :: (Num a, Bits a) => SyncSafe -> a
fromSyncSafe s =
  let s' = syncSafe s in BS.foldl (\a b -> shiftL a 7 .|. fromIntegral b) 0 s'

toSyncSafe :: (Integral a, Bits a) => a -> SyncSafe
toSyncSafe a = SyncSafe $ BS.singleton (fromIntegral (shiftR a 21 .&. 0x7F))
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
