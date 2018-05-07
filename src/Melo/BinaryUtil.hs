module Melo.BinaryUtil
  ( getUTF8Text
  , get24Bits
  ) where

import Data.Binary.Get
import Data.Bits
import Data.Text
import Data.Text.Encoding

getUTF8Text :: Int -> Get Text
getUTF8Text n = do
  bs <- getByteString n
  case decodeUtf8' bs of
    Left e -> fail $ "Error decoding string: " ++ show e
    Right s -> return s

get24Bits :: (Num a, Bits a) => Get a
get24Bits = do
  x <- fromIntegral <$> getWord8
  y <- fromIntegral <$> getWord8
  z <- fromIntegral <$> getWord8
  return $ shiftL x 16 .|. shiftL y 8 .|. z
