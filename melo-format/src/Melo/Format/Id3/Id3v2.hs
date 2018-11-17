{-# LANGUAGE AllowAmbiguousTypes #-}
module Melo.Format.Id3.Id3v2 where

import           Control.Monad
import           Control.Monad.Fail            as F
import           Data.Bifunctor
import           Data.Binary
import           Data.Binary.Get
import           Data.Bits
import           Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import           Data.Foldable                 as F
import           Data.Functor
import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.Encoding

import           Melo.Internal.Binary              hiding ( put )
import           Melo.Internal.BinaryUtil
import           Melo.Internal.Encoding
import           Melo.Internal.Locate
import           Melo.Internal.Format
import           Melo.Internal.Tag

data Id3v2 = Id3v2 {
  frameVersion :: Id3v2Version
, headerFlags :: HeaderFlags
, frames :: Frames
}

deriving instance Show Id3v2

instance MetadataFormat Id3v2 where
  formatDesc = "Id3v2"
  formatDesc' id3 = show $ frameVersion id3

instance MetadataLocator Id3v2 where
  locate bs = if L.isPrefixOf "ID3" bs then Just 0 else Nothing

instance TagReader Id3v2 where
  tags id3 = let frames' = frames id3
                 contents = foldlFrames accumFrames [] frames'
             in
    Tags $ fmap (first toTagKey) contents >>= \(a, bs) -> fmap ((a,)) bs
      where
        accumFrames acc frame = acc <> catMaybes [extractFrameContent frame]

instance BinaryGet Id3v2 where
  bget = do
    header <- getHeader
    when (hasExtendedHeader (flags header)) skipExtendedHeader
    Id3v2 (version header) (flags header) <$> getFrames header
      where
    skipExtendedHeader = skip =<< lookAhead (fromSyncSafe <$> bget)

data Header = Header {
  version :: Id3v2Version
, flags :: HeaderFlags
, totalSize :: SyncSafe
} deriving (Eq, Show)

instance BinaryGet Header where
  bget = do
    expectGetEq (getByteString 3) "ID3" "Expected ID3v2 identifier"
    header <- Header <$> bget <*> bget <*> bget
    expect (isReadable (flags header)) "Unrecognised ID3v2 flags found"
    pure header

getHeader :: Get Header
getHeader = bget

newtype Footer = Footer Header

instance BinaryGet Footer where
  bget = do
    expectGetEq (getByteString 3) "3DI" "Expected ID3v2 identifier"
    footer <- Header <$> bget <*> bget <*> bget
    expect (isReadable (flags footer)) "Unrecognised ID3v2 flags found"
    pure $ Footer footer

data Id3v2Version = Id3v24 | Id3v23
  deriving (Eq)

instance Show Id3v2Version where
  show = \case
    Id3v24 -> "ID3v2.4"
    Id3v23 -> "ID3v2.3"

instance BinaryGet Id3v2Version where
  bget = getWord16le >>= \case
    3 -> pure Id3v23
    4 -> pure Id3v24
    a -> F.fail $ "Unknown ID3v2 version " ++ show a

class Version (v :: Id3v2Version) where
  id3v2Version :: Id3v2Version

instance Version 'Id3v24 where
  id3v2Version = Id3v24

instance Version 'Id3v23 where
  id3v2Version = Id3v23

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

data Frames where
  Frames :: Version v => NonEmpty (Frame v) -> Frames

deriving instance Show Frames

foldlFrames :: (b -> forall v . Frame v -> b) -> b -> Frames -> b
foldlFrames f z (Frames frms) = F.foldl f z frms

type GetFrame v = (GetFrameHeader v, GetEncoding v, GetFrameId v, GetTextContent v, Version v)

getFrames :: Header -> Get Frames
getFrames header = do
  case version header of
    Id3v23 -> Frames <$> getFrames' @ 'Id3v23
    Id3v24 -> Frames <$> getFrames' @ 'Id3v24
 where
  flags' = flags header
  getFrames' :: GetFrame v => Get (NonEmpty (Frame (v :: Id3v2Version)))
  getFrames' = liftM2 (:|) bget getRest
  getRest :: GetFrame v => Get [Frame (v :: Id3v2Version)]
  getRest = isEnd >>= \case
    True  -> pure []
    False -> liftM2 (:) bget getRest
  isEnd = lookAhead (getByteString 1) <&> if hasFooter flags'
    then \case
-- found footer
      "3" -> True
      _   -> False
    else \case
-- found padding
      "\0" -> True
      _    -> False

data Frame (v :: Id3v2Version) = Frame (FrameContent v)
  deriving (Eq, Show)

instance GetFrame v => BinaryGet (Frame v) where
  bget = do
    header <- bget :: Get (FrameHeader v)
    case frameId header of
      (\a -> T.isPrefixOf "T" a || T.isPrefixOf "W" a -> True) -> do
        enc <- getEncoding @v
        fid <- getFrameId @v header enc
        Frame <$> mkFrameContent fid <$> getTextContent @v header fid enc
      _ -> Frame <$> OtherFrame <$> getByteString (fromIntegral $ frameSize header)

mkFrameContent :: FrameId -> [Text] -> FrameContent v
mkFrameContent fid t = case fid of
  PreDefinedId i    -> cid i
  UserDefinedId i _ -> cid i
 where
  cid i = case i of
    (T.isPrefixOf "T" -> True) -> TextFrame fid t
    (T.isPrefixOf "W" -> True) -> UrlFrame fid t
    _                          -> OtherFrame BS.empty

data FrameId =
    PreDefinedId Text
  | UserDefinedId Text Text
  deriving (Show, Eq)

toTagKey :: FrameId -> Text
toTagKey (PreDefinedId fid       ) = fid
toTagKey (UserDefinedId fid1 fid2) = fid1 <> ";" <> fid2

data FrameHeader (v :: Id3v2Version) = FrameHeader {
  frameId :: Text
, frameSize :: Word32
, frameFlags :: FrameHeaderFlags
}

deriving instance Eq (FrameHeader v)
deriving instance Show (FrameHeader v)

instance GetFrameHeader v => BinaryGet (FrameHeader v) where
  bget = getFrameHeader

class GetFrameHeader (v :: Id3v2Version) where
  getFrameHeader :: Get (FrameHeader v)

instance GetFrameHeader 'Id3v23 where
  getFrameHeader = FrameHeader <$> getUTF8Text 4 <*> getWord32be <*> bget

instance GetFrameHeader 'Id3v24 where
  getFrameHeader = FrameHeader <$> getUTF8Text 4 <*> (fromSyncSafe <$> bget) <*> bget

data FrameHeaderFlags = FrameHeaderFlags
  deriving (Eq, Show)

instance BinaryGet FrameHeaderFlags where
  bget = skip 2 >> pure FrameHeaderFlags

data FrameContent (v :: Id3v2Version) =
    TextFrame FrameId [Text]
  | UrlFrame FrameId [Text]
  | OtherFrame ByteString
  deriving (Show, Eq)

extractFrameContent :: Frame v -> Maybe (FrameId, [Text])
extractFrameContent (Frame content) = case content of
  TextFrame fid vs -> Just (fid, vs)
  UrlFrame  fid vs -> Just (fid, vs)
  _                -> Nothing

data TextEncoding = NullTerminated | UCS2 | UTF16 | UTF16BE | UTF8
  deriving (Show)

class GetEncoding (v :: Id3v2Version) where
  getEncoding :: Get TextEncoding

class GetTextContent (v :: Id3v2Version) where
  getTextContent :: FrameHeader v -> FrameId -> TextEncoding -> Get [Text]

class GetFrameId (v :: Id3v2Version) where
  getFrameId :: FrameHeader v -> TextEncoding -> Get FrameId

instance GetTextContent v where
  getTextContent header frameId enc = do
    let fidSz = case frameId of
              UserDefinedId _ t -> fromIntegral $ T.length t
              _ -> 0
    let sz = frameSize header - 1 - fidSz
    bs <- BS.dropWhile (== 0) <$> (getByteString $ fromIntegral sz)
    mapM (decodeId3Text enc) (splitFields (terminator enc) bs)

splitFields :: ByteString -> ByteString -> [ByteString]
splitFields term fields
  | BS.null fields = []
  | otherwise = h
  : if BS.null t then [] else splitFields term (BS.drop (BS.length term) t)
  where (h, t) = breakSubstring term fields

instance GetFrameId v where
  getFrameId header enc = do
    let fid = frameId header
    if T.isSuffixOf "XXX" fid then
      UserDefinedId fid <$> getUserDefinedFrameId header enc
    else pure $ PreDefinedId fid

getUserDefinedFrameId :: FrameHeader v -> TextEncoding -> Get Text
getUserDefinedFrameId header enc = do
  let sz = frameSize header
  bs <- lookAhead $ getByteString $ fromIntegral sz
  t  <- decodeId3Text enc $ fst (BS.breakSubstring (terminator enc) bs)
  skip $ T.length t
  pure t

instance GetEncoding 'Id3v23 where
  getEncoding = getWord8 >>= \case
    0 -> pure NullTerminated
    1 -> pure UCS2
    x -> F.fail $ "Unrecognised ID3v2.3 encoding flag " ++ show x

instance GetEncoding 'Id3v24 where
  getEncoding = getWord8 >>= \case
    0 -> pure NullTerminated
    1 -> pure UTF16
    2 -> pure UTF16BE
    3 -> pure UTF8
    x -> F.fail $ "Unrecognised ID3v2.4 encoding flag " ++ show x

terminator :: TextEncoding -> ByteString
terminator NullTerminated = "\0"
terminator UCS2           = "\0\0"
terminator UTF8           = "\0"
terminator UTF16          = "\0\0"
terminator UTF16BE        = "\0\0"

getId3Text :: TextEncoding -> Int -> Get Text
getId3Text NullTerminated _ = getNullTerminatedAscii
getId3Text UTF8           n = getUTF8Text n
getId3Text _              _ = undefined

decodeId3Text :: MonadFail m => TextEncoding -> ByteString -> m Text
decodeId3Text NullTerminated bs = pure $ decodeLatin1 bs
decodeId3Text UTF8           bs = decodeUtf8OrFail bs
decodeId3Text UTF16BE        bs = decodeUtf16BEOrFail bs
decodeId3Text UTF16          bs = decodeUtf16WithBOMOrFail bs
decodeId3Text UCS2           bs = decodeUtf16WithBOMOrFail bs

newtype SyncSafe = SyncSafe {
  syncSafe :: ByteString
} deriving (Eq, Show)

fromSyncSafe :: (Num a, Bits a) => SyncSafe -> a
fromSyncSafe s =
  let s' = syncSafe s
  in  BS.foldl (\a b -> shiftL a 7 .|. (fromIntegral b)) 0 s'

isSyncSafe :: ByteString -> Bool
isSyncSafe bs = not $ BS.any (\b -> testBit b 7) bs

instance Binary SyncSafe where
  get = do
    expectGet_ (lookAhead $ getByteString 4) isSyncSafe "Invalid sync safe integer"
    SyncSafe <$> getByteString 4
  put (SyncSafe s) = put s
