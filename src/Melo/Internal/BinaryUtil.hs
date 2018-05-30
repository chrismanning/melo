module Melo.Internal.BinaryUtil
  ( decodeUtf8OrFail
  , getUTF8Text
  , getNullTerminatedAscii
  , get24Bits
  , expect
  , expectGet
  , expectGet_
  , failFilter
  ) where

import Control.Monad
import Control.Monad.Fail as Fail
import Data.Binary
import Data.Binary.Get
import qualified Data.Binary.Bits.Get as BG
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Text as T
import Data.Text.Encoding

getUTF8Text :: Int -> Get Text
getUTF8Text n = getByteString n >>= decodeUtf8OrFail

decodeUtf8OrFail :: MonadFail m => ByteString -> m Text
decodeUtf8OrFail bs =
  case decodeUtf8' bs of
    Left e -> Fail.fail $ "Error decoding string: " ++ show e
    Right s -> return s

getNullTerminatedAscii :: Get Text
getNullTerminatedAscii = L.toStrict <$> getLazyByteStringNul >>= decodeUtf8OrFail

get24Bits :: Get Word32
get24Bits = BG.runBitGet $ BG.getWord32be 24

failFilter :: (MonadFail m) => (a -> Bool) -> String -> m a -> m a
failFilter p s ma = do
  a <- ma
  if p a then return a else Fail.fail s

expect :: (MonadFail m) => Bool -> String -> m ()
expect True _ = return ()
expect False s = Fail.fail s

expectGet :: Get t -> (t -> Bool) -> String -> Get t
expectGet g p s = do
  b <- p <$> lookAhead g
  if b then g else Fail.fail s

expectGet_ :: Get t -> (t -> Bool) -> String -> Get ()
expectGet_ g p s = void $ expectGet g p s
