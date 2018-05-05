module Melo.Vorbis where

import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.Bits
import Data.Text
import Data.Text.Encoding
import Prelude hiding (drop, length, take)

data FramedVorbisComments =
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
    vendorLength <- fromIntegral <$> getWord32le
    vendorBs <- getByteString vendorLength
    vendorString <-
      case decodeUtf8' vendorBs of
        Left e -> fail $ "Error decoding vorbis vendor string: " ++ show e
        Right s -> return s
    numComments <- fromIntegral <$> getWord32le
    VorbisComments vendorString <$> replicateM numComments get

data UserComment =
  UserComment Text
              Text
  deriving (Show, Eq)

instance Binary UserComment where
  put = undefined
  get = do
    commentLength <- fromIntegral <$> getWord32le
    commentBs <- getByteString commentLength
    Just (name, value) <-
      case decodeUtf8' commentBs of
        Left e -> fail $ "Error decoding vorbis comment: " ++ show e
        Right comment -> do
          return $ splitOnce (== '=') comment
    return $ UserComment name value

splitOnce :: (Char -> Bool) -> Text -> Maybe (Text, Text)
splitOnce p t = do
  n <- findIndex p t
  return $ (take n t, drop (min (length t) (n + 1)) t)
