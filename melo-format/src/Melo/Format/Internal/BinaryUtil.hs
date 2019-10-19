module Melo.Format.Internal.BinaryUtil
  ( get24Bits,
    getLazyByteStringUpTo,
    getNullTerminatedAscii,
    getUTF8Text,
    expect,
    expectGet,
    expectGet_,
    expectGetEq,
    hGetFileContents,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail as Fail
import Data.Binary
import qualified Data.Binary.Bits.Get as BG
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import Data.Text as T
import Melo.Format.Internal.Encoding
import System.IO
import Text.Printf

getUTF8Text :: Int -> Get Text
getUTF8Text n = getByteString n >>= decodeUtf8OrFail

getNullTerminatedAscii :: Get Text
getNullTerminatedAscii =
  L.toStrict <$> getLazyByteStringNul >>= decodeUtf8OrFail

get24Bits :: Get Word32
get24Bits = BG.runBitGet $ BG.getWord32be 24

expect :: (MonadFail m) => Bool -> String -> m ()
expect True _ = return ()
expect False s = Fail.fail s

expectGet :: Get t -> (t -> Bool) -> String -> Get t
expectGet g p s = do
  b <- p <$> lookAhead g
  if b then g else Fail.fail s

expectGet_ :: Get t -> (t -> Bool) -> String -> Get ()
expectGet_ g p s = void $ expectGet g p s

expectGetEq :: (Show t, Eq t) => Get t -> t -> String -> Get ()
expectGetEq g t s = do
  v <- g
  if v == t
    then return ()
    else Fail.fail $ printf "%s: expected '%s'; got '%s'" s (show t) (show v)

getLazyByteStringUpTo :: Int -> Get L.ByteString
getLazyByteStringUpTo n =
  getLazyByteString (fromIntegral n) <|> getRemainingLazyByteString

hGetFileContents :: Handle -> IO L.ByteString
hGetFileContents h = do
  len <- hFileSize h
  L.hGet h (fromIntegral len)
