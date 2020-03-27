{-# LANGUAGE UndecidableInstances #-}

module Melo.Format.Internal.Binary
  ( BinaryGet (..),
    bdecode,
    bdecodeOrFail,
    bdecodeFile,
    bdecodeFileOrFail,
    bdecodeOrThrowIO,
  )
where

import Control.Exception.Safe
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import Data.Binary.Get (Decoder (..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
  ( defaultChunkSize,
  )
import qualified Data.Text as T
import Melo.Format.Error (MetadataException (MetadataReadError))
import System.IO

class BinaryGet a where
  bget :: Bin.Get a

bdecode :: BinaryGet a => L.ByteString -> a
bdecode = Bin.runGet bget

bdecodeOrFail :: BinaryGet a => L.ByteString -> Either (L.ByteString, Bin.ByteOffset, String) (L.ByteString, Bin.ByteOffset, a)
bdecodeOrFail = Bin.runGetOrFail bget

bdecodeFile :: BinaryGet a => FilePath -> IO a
bdecodeFile f = do
  result <- bdecodeFileOrFail f
  case result of
    Right x -> return x
    Left (_, str) -> error str

bdecodeFileOrFail :: BinaryGet a => FilePath -> IO (Either (Bin.ByteOffset, String) a)
bdecodeFileOrFail f =
  withBinaryFile f ReadMode $ \h -> feed (Bin.runGetIncremental bget) h
  where
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos str) _ = return (Left (pos, str))
    feed (Partial k) h = do
      chunk <- B.hGet h L.defaultChunkSize
      case B.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h

bdecodeOrThrowIO :: BinaryGet a => L.ByteString -> IO a
bdecodeOrThrowIO buf = case Bin.runGetOrFail bget buf of
  Left (_, _, s) -> throwIO $ MetadataReadError (T.pack s)
  Right (_, _, a) -> pure a

instance {-# OVERLAPPABLE #-} Bin.Binary a => BinaryGet a where
  bget = Bin.get
