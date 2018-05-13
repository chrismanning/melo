module Melo.BinaryUtil
  ( getUTF8Text
  , get24Bits
  , expect
  , failFilter
  ) where

import Control.Monad.Fail
import Data.Binary.Get
import qualified Data.Binary.Bits.Get as BG
import Data.Text
import Data.Text.Encoding
import Data.Word

import Prelude hiding (fail)

getUTF8Text :: Int -> Get Text
getUTF8Text n = do
  bs <- getByteString n
  case decodeUtf8' bs of
    Left e -> fail $ "Error decoding string: " ++ show e
    Right s -> return s

get24Bits :: Get Word32
get24Bits = BG.runBitGet $ BG.getWord32be 24

failFilter :: (MonadFail m) => (a -> Bool) -> String -> m a -> m a
failFilter p s ma = do
  a <- ma
  if p a then return a else fail s

expect :: (MonadFail m) => Bool -> String -> m ()
expect True _ = return ()
expect False s = fail s
