module Melo.BinaryUtil
  ( failif
  , getUTF8Text
  , get24Bits
  , mustMatch
  , mustSatisfy
  ) where

import Control.Monad.Fail
import Data.Binary.Get
import Data.Bits
import Data.Text
import Data.Text.Encoding
import Text.Printf

import Prelude hiding (fail)

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

failif :: (MonadFail m) => Bool -> String -> m ()
failif b s =
  if b
    then return ()
    else fail s

mustMatch :: (MonadFail m, Eq a, Show a) => a -> a -> String -> m ()
mustMatch a b s =
  if a == b
    then return ()
    else fail (printf "%s: expected %s\n\t got %s" s (show a) (show b))

mustSatisfy :: (MonadFail m, Show a) => (a -> Bool) -> a -> String -> m ()
mustSatisfy p a s =
  if p a
    then return ()
    else fail (s ++ ": " ++ show a)
