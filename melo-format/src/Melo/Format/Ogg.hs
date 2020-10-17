module Melo.Format.Ogg where

import Control.Monad
import Control.Monad.ST.Strict
import Data.Array.Unboxed
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.List.Split
import Data.STRef
import Data.String
import Melo.Format.Internal.BinaryUtil

data OggPage a = OggPage
  { header :: !PageHeader,
    page :: !a,
    otherSegments :: ![BS.ByteString]
  }
  deriving (Functor)

deriving instance Eq a => Eq (OggPage a)

deriving instance Show a => Show (OggPage a)

instance Binary a => Binary (OggPage a) where
  get = do
    Page {..} <- get
    case runGetOrFail get $ L.fromStrict (head pageData) of
      Right (_, _, a) -> pure $ OggPage pageHeader a (drop 1 pageData)
      Left (_, _, s) -> fail s
  put OggPage {..} =
    put $
      Page header (L.toStrict (runPut (put page)) : otherSegments)

capturePattern :: IsString s => s
capturePattern = "OggS"

data Page = Page
  { pageHeader :: !PageHeader,
    pageData :: ![BS.ByteString]
  }

instance Binary Page where
  get = do
    pageHeader <- get
    _checksum <- getWord32le
    numSegments <- getWord8
    segmentTable <- getByteString $ fromIntegral numSegments
    let pageLengths' = pageLengths (BS.unpack segmentTable)
    pages <- mapM getByteString pageLengths'
    pure $ Page pageHeader pages
    where
      pageLengths :: [Word8] -> [Int]
      pageLengths table = sum . fmap fromIntegral <$> split (keepDelimsR $ dropFinalBlank $ whenElt (/= 255)) table
  put page = do
    let checksum = crc32 (runPut (putPage page 0))
    putPage page checksum

putPage :: Page -> Word32 -> Put
putPage Page {..} checksum = do
  put pageHeader
  putWord32le checksum
  let pageLengths = BS.length <$> pageData
  segmentTable <- forM pageLengths $ \pageLength -> do
    let (numSegments, remainder) = pageLength `divMod` 0xFF
    pure $ BS.pack (replicate numSegments 0xFF) <> BS.pack [fromIntegral remainder | remainder /= 0]
  putWord8 (fromIntegral $ sum . fmap BS.length $ segmentTable)
  mapM_ putByteString segmentTable
  mapM_ putByteString pageData

data PageHeader = PageHeader
  { typeFlags :: !Word8,
    position :: !Int64,
    serialNum :: !Word32,
    sequenceNum :: !Word32
  }
  deriving (Show, Eq)

instance Binary PageHeader where
  get = do
    expectGetEq (getByteString 4) capturePattern "Ogg capture pattern expected"
    expectGetEq getWord8 0 "Ogg stream structure version 0 expected"
    PageHeader <$> getWord8 <*> getInt64le <*> getWord32le <*> getWord32le
  put PageHeader {..} = do
    putByteString capturePattern
    -- version
    putWord8 0
    putWord8 typeFlags
    putInt64le position
    putWord32le serialNum
    putWord32le sequenceNum

-- crc32 generation based on code from
-- https://web.archive.org/web/20200403182132/http://www.ross.net/crc/download/crc_v3.txt

crc32 :: L.ByteString -> Word32
crc32 = crc32Update 0x00

crc32Update :: Word32 -> L.ByteString -> Word32
crc32Update crcInit d = runST $ do
  crc <- newSTRef crcInit
  go d crc
  readSTRef crc
  where
    go d' crc
      | L.null d' = pure ()
      | otherwise = do
        let (x, xs) = (L.head d', L.tail d')
        crc' <- readSTRef crc
        let ix = fromIntegral x `xor` ((crc' `shiftR` 24) .&. 0xFF)
        writeSTRef crc $ (crcLookup ! ix) `xor` (crc' `shiftL` 8)
        go xs crc

crcLookup :: UArray Word32 Word32
crcLookup = listArray (0, 255) (genEntry <$> [0 .. 255])

genEntry :: Word32 -> Word32
genEntry idx = runST $ do
  r <- newSTRef (idx `shiftL` 24)
  replicateM_ 8 $ do
    r' <- readSTRef r
    if r' .&. 0x80000000 /= 0
      then writeSTRef r ((r' `shiftL` 1) `xor` 0x04C11DB7)
      else writeSTRef r (r' `shiftL` 1)
  r' <- readSTRef r
  pure (r' .&. 0xFFFFFFFF)
