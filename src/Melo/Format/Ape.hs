module Melo.Format.Ape where

import Control.Monad
import Data.Binary
import Data.Binary.Bits
import Data.Binary.Bits.Get ()
import qualified Data.Binary.Bits.Get as BG
import qualified Data.Binary.Bits.Put as BP
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString as BS
import Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Text
import Data.Text.Encoding

import Prelude as P

import Melo.Internal.BinaryUtil

data APE = APE
  { version :: Version
  , items :: [TagItem]
  } deriving (Show, Eq)

instance Binary APE where
  put a = do
    let bs = runPut $ forM_ (items a) put
    if version a == APEv2 then putHeader a $ fromIntegral . L.length $ bs
      else return ()
    putLazyByteString bs
    putFooter a $ fromIntegral . L.length $ bs
  get = do
    expectGet_ (getByteString 8) (== "APETAGEX") "Not an APEv2 header"
    version <- get
    numBytes <- fromIntegral <$> getWord32le
    numItems <- fromIntegral <$> getWord32le
    _ <- get :: Get Flags
    skip 8
    items <- replicateM numItems get
    skip 32
    bs <- bytesRead
    expect (bs == (numBytes + 32)) "Inconsistent APE header tag size"
    return $ APE version items

putHeader :: APE -> Word32 -> Put
putHeader a n = putHeader_ a n $ Flags True False True TextItemType False

putFooter :: APE -> Word32 -> Put
putFooter a n = putHeader_ a n $ Flags True False False TextItemType False

putHeader_ :: APE -> Word32 -> Flags -> Put
putHeader_ a n f = do
  putByteString $ BC.pack "APETAGEX"
  put $ version a
  putWord32le $ fromIntegral (n + 32)
  putWord32le $ fromIntegral (P.length $ items a)
  put f
  putWord64be 0

data Version
  = APEv1
  | APEv2
  deriving (Show, Eq)

instance Binary Version where
  put v =
    case v of
      APEv1 -> putWord32le 1000
      APEv2 -> putWord32le 2000
  get =
    getWord32le >>= \case
      1000 -> return APEv1
      2000 -> return APEv2
      x -> fail $ "Invalid APE tag version " ++ show x

data Flags = Flags
  { hasHeader :: Bool
  , hasFooter :: Bool
  , isHeader :: Bool
  , itemType :: TagItemType
  , readOnly :: Bool
  } deriving (Show, Eq)

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
  get = do
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

data TagItem =
  TagItem Text
          TagItemValue
  deriving (Show, Eq)

mkTextTagItem :: Text -> Text -> TagItem
mkTextTagItem k v = TagItem k (TagItemValue (TextTag [v]))

instance Binary TagItem where
  put (TagItem key val) = do
    let bs = runPut $ putTagItemValue val
    putWord32le $ fromIntegral . L.length $ bs
    put $ mkItemFlags val
    putByteString $ encodeUtf8 key
    putByteString "\0"
    putLazyByteString bs
  get = do
    valueSize <- fromIntegral <$> getWord32le
    itemFlags <- get :: Get Flags
    key <- getNullTerminatedAscii
    val <- getTagItemValue (itemType itemFlags) valueSize
    return $ TagItem key val

data TagItemValue where
  TagItemValue :: TagValue a b -> TagItemValue

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
    t =
      case v of
        TextTag _ -> TextItemType
        BinaryTag _ -> BinaryItemType
        ExternalLocatorTag _ -> ExternalLocatorItemType
        ReservedTag _ -> ReservedItemType

data TagValue a b where
  TextTag :: [Text] -> TagValue 'TextItemType [Text]
  BinaryTag :: ByteString -> TagValue 'BinaryItemType ByteString
  ExternalLocatorTag :: [Text] -> TagValue 'ExternalLocatorItemType [Text]
  ReservedTag :: ByteString -> TagValue 'ReservedItemType ByteString

deriving instance Show (TagValue a b)

deriving instance Eq b => Eq (TagValue a b)

getTagItemValue :: TagItemType -> Int -> Get TagItemValue
getTagItemValue t n =
  case t of
    TextItemType -> TagItemValue . TextTag <$> getValueList n
    BinaryItemType -> TagItemValue . BinaryTag <$> getByteString n
    ExternalLocatorItemType ->
      TagItemValue . ExternalLocatorTag <$> getValueList n
    ReservedItemType -> TagItemValue . ReservedTag <$> getByteString n

getValueList :: Int -> Get [Text]
getValueList n = do
  bs <- getByteString n
  let vals = BS.splitWith (== 0) bs
  forM vals decodeUtf8OrFail

putTagItemValue :: TagItemValue -> Put
putTagItemValue (TagItemValue t) =
  case t of
    TextTag vals -> putValueList vals
    BinaryTag val -> putByteString val
    ExternalLocatorTag vals -> putValueList vals
    ReservedTag val -> putByteString val

putValueList :: [Text] -> Put
putValueList vals =
  putByteString $ BS.intercalate (BC.pack "\0") $ fmap encodeUtf8 vals
