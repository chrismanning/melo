module Melo.Format.Ape
  ( APEv1 (..),
    APEv2 (..),
    Header (..),
    Flags (..),
    Version (..),
    headerSize,
    preamble,
    getHeader,
    mkTextTagItem,
    pattern TextTagItem,
    apeV1Id,
    apeV2Id,
    apeTag,
  )
where

import Control.Monad
import Data.Binary
import Data.Binary.Bits
import qualified Data.Binary.Bits.Get as BG
import qualified Data.Binary.Bits.Put as BP
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Coerce
import Data.Int
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Melo.Format.ID3 as ID3
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Encoding
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import Numeric.Natural (Natural)
import System.IO
import Prelude as P

apeTag :: TagMapping -> TagLens
apeTag = mappedTag ape

newtype APEv1 = APEv1 (Vector TagItem)
  deriving (Show, Eq)

instance MetadataFormat APEv1 where
  metadataFormat =
    MetadataFormat
      { formatId = apeV1Id,
        formatDesc = "APEv1"
      }
  metadataLens = apeTag

apeV1Id :: MetadataId
apeV1Id = MetadataId "APEv1"

instance TagReader APEv1 where
  readTags a = Tags (coerce a >>= getTextItem)

instance Binary APEv1 where
  get = do
    ape <- get @APE
    pure $ APEv1 (items ape)
  put a =
    put $
      APE
        { version = V1,
          items = coerce a
        }

instance MetadataLocator APEv1 where
  locate bs = locateApe bs V1
  hLocate h = hLocateApe h V1

locateApe :: L.ByteString -> Version -> Maybe Int
locateApe bs v =
  case locateBinaryLazy @Header bs of
    Nothing -> Nothing
    Just i -> do
      let header = runGet (lookAhead $ skip (fromIntegral i) >> getHeader) bs
      if headerVersion header /= v
        then Nothing
        else
          Just $
            if isHeader (flags header)
              then i
              else i - fromIntegral (numBytes header) - headerSize

hLocateApe :: Num a => Handle -> Version -> IO (Maybe a)
hLocateApe h v = do
  hs <-
    do
      hSeek h SeekFromEnd (fromIntegral ID3.headerSize + headerSize)
      hTell h
  let n = hs - fromIntegral (min (headerSize * 10) hs)
  hSeek h AbsoluteSeek n
  buf <- BS.hGet h (fromIntegral $ hs - n)
  case fromIntegral <$> locateApe (L.fromStrict buf) v of
    Just loc -> pure $ Just (fromIntegral (loc + n))
    Nothing -> pure Nothing

newtype APEv2 = APEv2 (Vector TagItem)
  deriving (Show, Eq)

instance MetadataFormat APEv2 where
  metadataFormat =
    MetadataFormat
      { formatId = apeV2Id,
        formatDesc = "APEv2"
      }
  metadataLens = apeTag

apeV2Id :: MetadataId
apeV2Id = MetadataId "APEv2"

instance TagReader APEv2 where
  readTags a = Tags (coerce a >>= getTextItem)

getTextItem :: TagItem -> Vector (Text, Text)
getTextItem t = case t of
  TagItem k (TagItemValue (TextTag vs)) -> fmap (k,) vs
  _ -> V.empty

instance Binary APEv2 where
  get = do
    ape <- get @APE
    pure $ APEv2 (items ape)
  put a =
    put $
      APE
        { version = V2,
          items = coerce a
        }

instance MetadataLocator APEv2 where
  locate bs = locateApe bs V2
  hLocate h = hLocateApe h V2

data APE = APE
  { version :: !Version,
    items :: !(Vector TagItem)
  }
  deriving (Show, Eq)

headerSize :: Integral a => a
headerSize = 32

preamble :: BS.ByteString
preamble = "APETAGEX"

instance Binary APE where
  put a = do
    let bs = runPut $ V.forM_ (items a) put
    case version a of
      V2 -> put $ mkHeader a $ fromIntegral . L.length $ bs
      _ -> return ()
    putLazyByteString bs
    put $ mkFooter a $ fromIntegral . L.length $ bs

  get = do
    i <-
      findHeader >>= \case
        Nothing -> fail "APE tags not found"
        Just i -> fromIntegral <$> return i
    header <- lookAhead (skip i >> getHeader)
    let bytes = fromIntegral (numBytes header)
    if isHeader (flags header)
      then skip $ i + headerSize
      else skip $ i - bytes + headerSize
    items <-
      isolate bytes $ do
        items <- V.replicateM (fromIntegral . numItems $ header) getTagItem
        _ <- getHeader
        return items
    return $ APE {items, version = headerVersion header}

data Header = Header
  { headerVersion :: !Version,
    numBytes :: !Word32,
    numItems :: !Word32,
    flags :: !Flags
  }
  deriving (Show, Eq)

type Footer = Header

instance Binary Header where
  put = putHeader

  get = getHeader

findHeader :: Get (Maybe Natural)
findHeader = lookAhead $ findByChunk 1024 0
  where
    findByChunk :: Int -> Int -> Get (Maybe Natural)
    findByChunk c i = do
      bs <- mfilter (not . L.null) $ getLazyByteStringUpTo c
      case locateBinaryLazy @Header bs of
        Nothing -> findByChunk c (i + (c `div` 2))
        Just x -> return $ Just $ fromIntegral (x + i)

mkHeader :: APE -> Word32 -> Header
mkHeader a n = mkHeader_ a n $ Flags True False True TextItemType False

mkFooter :: APE -> Word32 -> Footer
mkFooter a n = mkHeader_ a n $ Flags True False False TextItemType False

mkHeader_ :: APE -> Word32 -> Flags -> Header
mkHeader_ a n f =
  Header
    { headerVersion = version a,
      numBytes = n + headerSize,
      numItems = fromIntegral (P.length $ items a),
      flags = f
    }

putHeader :: Header -> Put
putHeader h = do
  putByteString preamble
  put $ headerVersion h
  putWord32le $ fromIntegral (numBytes h)
  putWord32le $ fromIntegral (numItems h)
  put $ flags h
  putWord64be 0

getHeader :: Get Header
getHeader = do
  expectGetEq
    (getByteString (BS.length preamble))
    preamble
    "Invalid APE preamble"
  h <- Header <$> getVersion <*> getWord32le <*> getWord32le <*> getFlags
  skip 8
  return h

data Version
  = V1
  | V2
  deriving (Show, Eq)

instance Binary Version where
  put v =
    case v of
      V1 -> putWord32le 1000
      V2 -> putWord32le 2000

  get = getVersion

getVersion :: Get Version
getVersion = getWord32le >>= \case
  1000 -> return V1
  2000 -> return V2
  x -> fail $ "Invalid APE tag version " <> show x

data Flags = Flags
  { hasHeader :: !Bool,
    hasFooter :: !Bool,
    isHeader :: !Bool,
    itemType :: !TagItemType,
    readOnly :: !Bool
  }
  deriving (Show, Eq)

instance Binary Flags where
  put f =
    BP.runBitPut $ do
      BP.putWord8 5 0
      putBits 2 $ itemType f
      BP.putBool $ readOnly f
      BP.putWord32be 16 0
      BP.putBool $ hasHeader f
      BP.putBool $ hasFooter f
      BP.putBool $ isHeader f
      BP.putWord8 5 0

  get = getFlags

getFlags :: Get Flags
getFlags = do
  (itemType, readOnly) <-
    BG.runBitGet $ (,) <$> (BG.getWord8 5 *> getBits 2) <*> BG.getBool
  skip 2
  (hasHeader, hasFooter, isHeader) <-
    BG.runBitGet $ (,,) <$> BG.getBool <*> BG.getBool <*> BG.getBool
  return Flags {hasHeader, hasFooter, isHeader, itemType, readOnly}

data TagItemType
  = TextItemType
  | BinaryItemType
  | ExternalLocatorItemType
  | ReservedItemType
  deriving (Show, Eq, Enum)

instance BinaryBit TagItemType where
  putBits 2 t = BP.putWord8 2 (fromIntegral . fromEnum $ t)
  putBits _ _ = fail "Invalid tag item type size"

  getBits 2 = toEnum . fromIntegral <$> BG.getWord8 2
  getBits _ = fail "Invalid tag item type size"

data TagItem
  = TagItem
      !Text
      !TagItemValue
  deriving (Show, Eq)

mkTextTagItem :: Text -> Text -> TagItem
mkTextTagItem k v = TagItem k (TagItemValue (TextTag (V.singleton v)))

pattern TextTagItem ::
  forall (a :: TagItemType) b.
  (a ~ 'TextItemType, b ~ Vector Text) =>
  Text ->
  Vector Text ->
  TagItem
pattern TextTagItem k vs = TagItem k (TagItemValue (TextTag vs))

instance Binary TagItem where
  put (TagItem key val) = do
    let bs = runPut $ putTagItemValue val
    putWord32le $ fromIntegral . L.length $ bs
    put $ mkItemFlags val
    putByteString $ encodeUtf8 key
    putByteString "\0"
    putLazyByteString bs

  get = getTagItem

getTagItem :: Get TagItem
getTagItem = do
  valueSize <- fromIntegral <$> getWord32le
  itemFlags <- get :: Get Flags
  key <- getNullTerminatedAscii
  val <- getTagItemValue (itemType itemFlags) valueSize
  return $ TagItem key val

data TagItemValue where
  TagItemValue :: !(TagValue a b) -> TagItemValue

deriving instance Show TagItemValue

instance Eq TagItemValue where
  TagItemValue (TextTag a) == TagItemValue (TextTag b) = a == b
  TagItemValue (BinaryTag a) == TagItemValue (BinaryTag b) = a == b
  TagItemValue (ExternalLocatorTag a) == TagItemValue (ExternalLocatorTag b) =
    a == b
  TagItemValue (ReservedTag a) == TagItemValue (ReservedTag b) = a == b
  (==) _ _ = False

mkItemFlags :: TagItemValue -> Flags
mkItemFlags (TagItemValue v) = Flags False False False t False
  where
    t = case v of
      TextTag _ -> TextItemType
      BinaryTag _ -> BinaryItemType
      ExternalLocatorTag _ -> ExternalLocatorItemType
      ReservedTag _ -> ReservedItemType

data TagValue a b where
  TextTag :: !(Vector Text) -> TagValue 'TextItemType (Vector Text)
  BinaryTag :: !BS.ByteString -> TagValue 'BinaryItemType BS.ByteString
  ExternalLocatorTag :: !(Vector Text) -> TagValue 'ExternalLocatorItemType (Vector Text)
  ReservedTag :: !BS.ByteString -> TagValue 'ReservedItemType BS.ByteString

deriving instance Show (TagValue a b)

deriving instance Eq b => Eq (TagValue a b)

getTagItemValue :: TagItemType -> Int -> Get TagItemValue
getTagItemValue t n = case t of
  TextItemType -> TagItemValue . TextTag <$> getValueList n
  BinaryItemType -> TagItemValue . BinaryTag <$> getByteString n
  ExternalLocatorItemType ->
    TagItemValue . ExternalLocatorTag <$> getValueList n
  ReservedItemType -> TagItemValue . ReservedTag <$> getByteString n

getValueList :: Int -> Get (Vector Text)
getValueList n = do
  bs <- getByteString n
  let vals = V.fromList $ BS.splitWith (== 0) bs
  V.forM vals decodeUtf8OrFail

putTagItemValue :: TagItemValue -> Put
putTagItemValue (TagItemValue t) = case t of
  TextTag vals -> putValueList vals
  BinaryTag val -> putByteString val
  ExternalLocatorTag vals -> putValueList vals
  ReservedTag val -> putByteString val

putValueList :: Vector Text -> Put
putValueList vals =
  putByteString $ BS.intercalate (BC.pack "\0") $ V.toList (fmap encodeUtf8 vals)
