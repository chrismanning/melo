module Melo.Vorbis where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Text
import Prelude hiding (drop, length, take)

import Melo.BinaryUtil

newtype FramedVorbisComments =
  FramedVorbisComments VorbisComments
  deriving (Show, Eq)

instance Binary FramedVorbisComments where
  put = undefined
  get = do
    vc <- get
    next <- lookAhead getWord8
    guard $ testBit next 7
    return $ FramedVorbisComments vc

data VorbisComments =
  VorbisComments Text
                 [UserComment]
  deriving (Show, Eq)

instance Binary VorbisComments where
  put = undefined
  get = do
    vendorString <- getUTF8Text =<< fromIntegral <$> getWord32le
    numComments <- fromIntegral <$> getWord32le
    VorbisComments vendorString <$> replicateM numComments get

data UserComment =
  UserComment Text
              Text
  deriving (Show, Eq)

instance Binary UserComment where
  put = undefined
  get = do
    comment <- getUTF8Text =<< fromIntegral <$> getWord32le
    Just (name, value) <- return $ splitOnce (== '=') comment
    return $ UserComment name value

splitOnce :: (Char -> Bool) -> Text -> Maybe (Text, Text)
splitOnce p t = do
  n <- findIndex p t
  return (take n t, drop (min (length t) (n + 1)) t)
