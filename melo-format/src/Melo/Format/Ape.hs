{-# LANGUAGE AllowAmbiguousTypes #-}

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
    hSkip,
    hGetApe,
  )
where

import Control.Exception.Safe
import Control.Monad
import Data.Binary
import Data.Binary.Bits
import qualified Data.Binary.Bits.Get as BG
import qualified Data.Binary.Bits.Put as BP
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Coerce
import Data.Functor
import Data.Text (Text)
import Data.Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Melo.Format.ID3 as ID3
import Melo.Format.Error
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Encoding
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
import Lens.Micro
import qualified Streaming.Binary as S
import qualified Streaming.ByteString as S
import Numeric.Natural (Natural)
import System.IO

apeTag :: TagMapping -> TagLens
apeTag = mappedTag ape

newtype APEv1 = APEv1 (Vector TagItem)
  deriving (Show, Eq)

instance MetadataFormat APEv1 where
  metadataFormat =
    MetadataFormatDesc
      { formatId = apeV1Id,
        formatDesc = "APEv1"
      }
  fieldMappingSelector = ape
  readTags a = Tags (coerce a >>= getTextItem)
  metadataSize = toInteger . L.length . runPut . put
  replaceWithTags _ = APEv1 . createApeTags

apeV1Id :: MetadataId
apeV1Id = MetadataId "APEv1"

instance Binary APEv1 where
  get = do
    ape <- get @APE
    when (version ape /= V1) $
      fail "Expected APEv1 got APEv2"
    pure $ APEv1 (items ape)
  put a =
    put $
      APE
        { version = V1,
          items = coerce a
        }

type family ApeVersion (v :: Version) = t | t -> v where
  ApeVersion 'V1 = APEv1
  ApeVersion 'V2 = APEv2

class ApeVersion' (v :: Version) | v -> v where
  version' :: Version

instance ApeVersion' 'V1 where
  version' = V1

instance ApeVersion' 'V2 where
  version' = V2

hGetApe :: forall v. ApeVersion' v => Binary (ApeVersion v) => Handle -> IO (Maybe (ApeVersion v))
hGetApe h = do
  total <- hFileSize h
  ID3.hGetId3v1 h >>= \case
    Just _ ->
      hSeek h AbsoluteSeek (total - min total (fromIntegral ID3.id3v1Size + (headerSize * 10)))
    Nothing -> hSeek h AbsoluteSeek (total - min total (headerSize * 10))
  i <- hTell h
  findSubstring preamble (S.hGetContents h) <&> fmap fromIntegral >>= \case
    Nothing -> pure Nothing
    Just headerLoc -> do
      hSeek h AbsoluteSeek (i + headerLoc)
      (_, _, r) <- S.decodeWith getHeader (S.hGetContents h)
      case r of
        Left _ -> pure Nothing
        Right header | headerVersion header /= version' @v -> pure Nothing
                     | isHeader (flags header) -> do
                        hSeek h AbsoluteSeek (i + headerLoc)
                        S.decode (S.hGetContents h) <&> (^? _3 . _Right)
                     | otherwise -> do
                        let pos = i + headerLoc - fromIntegral (numBytes header) + headerSize
                        hSeek h AbsoluteSeek pos
                        S.decode (S.hGetContents h) <&> (^? _3 . _Right)

newtype APEv2 = APEv2 (Vector TagItem)
  deriving (Show, Eq)

instance MetadataFormat APEv2 where
  metadataFormat =
    MetadataFormatDesc
      { formatId = apeV2Id,
        formatDesc = "APEv2"
      }
  fieldMappingSelector = ape
  readTags a = Tags (coerce a >>= getTextItem)
  metadataSize = toInteger . L.length . runPut . put
  replaceWithTags _ = APEv2 . createApeTags

apeV2Id :: MetadataId
apeV2Id = MetadataId "APEv2"

getTextItem :: TagItem -> Vector (Text, Text)
getTextItem t = case t of
  TagItem k (TagItemValue (TextTag vs)) -> fmap (k,) vs
  _otherwise -> V.empty

instance Binary APEv2 where
  get = do
    ape <- get @APE
    when (version ape /= V2) $
      fail "Expected APEv2 got APEv1"
    pure $ APEv2 (items ape)
  put a =
    put $
      APE
        { version = V2,
          items = coerce a
        }

data APE = APE
  { version :: !Version,
    items :: !(Vector TagItem)
  }
  deriving (Show, Eq)

headerSize :: Integral a => a
headerSize = 32

preamble :: BS.ByteString
preamble = "APETAGEX"

hSkip :: Handle -> IO ()
hSkip h = do
  buf <- BS.hGet h (BS.length preamble)
  hSeek h RelativeSeek (negate (fromIntegral (BS.length buf)))
  when (buf == preamble) do
    runGetOrFail getHeader <$> L.hGet h headerSize >>= \case
      Left _ -> impureThrow $ MetadataReadError "found 'APETAGEX' identifier but failed to read APE header"
      Right (_, _, header) -> hSeek h RelativeSeek (fromIntegral header.numBytes)

instance Binary APE where
  put a = do
    let bs = runPut $ V.forM_ (items a) put
    case version a of
      V2 -> put $ mkHeader a $ fromIntegral . L.length $ bs
      V1 -> return ()
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
    findByChunk :: Int -> Natural -> Get (Maybe Natural)
    findByChunk c i = do
      bs <- mfilter (not . L.null) $ getLazyByteStringUpTo c
      findSubstring preamble (S.fromLazy bs) >>= \case
        Nothing -> findByChunk c (i + fromIntegral (c `div` 2))
        Just x -> return $ Just (x + i)

mkHeader :: APE -> Word32 -> Header
mkHeader a n = mkHeader_ a n $ Flags True False True TextItemType False

mkFooter :: APE -> Word32 -> Footer
mkFooter a n = mkHeader_ a n $ Flags True False False TextItemType False

mkHeader_ :: APE -> Word32 -> Flags -> Header
mkHeader_ a n f =
  Header
    { headerVersion = version a,
      numBytes = n + headerSize,
      numItems = fromIntegral (V.length $ items a),
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
getVersion =
  getWord32le >>= \case
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
  putByteString $ BS.intercalate "\0" $ V.toList (fmap encodeUtf8 vals)

createApeTags :: Tags -> Vector TagItem
createApeTags (Tags tags) = fmap (uncurry mkTextTagItem) tags
