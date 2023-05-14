{-# LANGUAGE UndecidableInstances #-}

module Melo.Format.Internal.Binary
  ( BinaryGet (..),
    bdecodeOrFail,
    bdecodeStreamOrFail,
    bdecodeHandleOrFail,
    bdecodeFileOrFail,
    bdecodeOrThrowIO,
    hSkipZeroes,
  )
where

import Control.Exception.Safe
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.ByteString.Lazy as L
import Data.ByteString qualified as BS
import qualified Data.Text as T
import Melo.Format.Error (MetadataException (MetadataReadError))
import Streaming.Binary qualified as S
import Streaming.ByteString qualified as S
import System.IO

class BinaryGet a where
  bget :: Bin.Get a

bdecodeOrFail :: BinaryGet a => L.ByteString -> Either (L.ByteString, Bin.ByteOffset, String) (L.ByteString, Bin.ByteOffset, a)
bdecodeOrFail = Bin.runGetOrFail bget

bdecodeStreamOrFail :: BinaryGet a => S.ByteStream IO r -> IO (Either (Bin.ByteOffset, String) a)
bdecodeStreamOrFail stream = do
  (_, i, r) <- S.decodeWith bget stream
  case r of
    Left e -> pure $ Left (i, e)
    Right a -> pure $ Right a

bdecodeHandleOrFail :: BinaryGet a => Handle -> IO (Either (Bin.ByteOffset, String) a)
bdecodeHandleOrFail = bdecodeStreamOrFail . S.hGetContents

bdecodeFileOrFail :: BinaryGet a => FilePath -> IO (Either (Bin.ByteOffset, String) a)
bdecodeFileOrFail f = withBinaryFile f ReadMode bdecodeHandleOrFail

bdecodeOrThrowIO :: (MonadThrow m, BinaryGet a) => S.ByteStream m r -> m a
bdecodeOrThrowIO stream = do
  (_, _, r) <- S.decodeWith bget stream
  case r of
    Left e -> throwIO $ MetadataReadError (T.pack e)
    Right !a -> pure a

instance {-# OVERLAPPABLE #-} Bin.Binary a => BinaryGet a where
  bget = Bin.get

hSkipZeroes :: Handle -> IO ()
hSkipZeroes h = do
  buf <- BS.hGetSome h 1
  if buf BS.!? 0 == Just 0
    then hSkipZeroes h
    else hSeek h RelativeSeek (fromIntegral (negate (BS.length buf)))
