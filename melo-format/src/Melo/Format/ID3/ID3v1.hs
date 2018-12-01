module Melo.Format.ID3.ID3v1
  ( ID3v1(..)
  , id3v1Id
  )
where

import           Control.Monad
import           Control.Monad.Fail            as F
import           Data.Binary
import           Data.Binary.Get
import qualified Data.ByteString               as BS
import           Data.Char
import           Data.List                                ( genericIndex
                                                          , genericLength
                                                          )
import           Data.Text                     as T
import           Data.Text.Encoding
import           System.IO

import           Melo.Format.ID3.ID3v1Genre
import           Melo.Format.Internal.Binary
import           Melo.Format.Internal.BinaryUtil
import           Melo.Format.Internal.Format
import           Melo.Format.Internal.Locate
import           Melo.Format.Internal.Tag

data ID3v1 = ID3v1
  { title   :: Text
  , artist  :: Text
  , album   :: Text
  , year    :: Text
  , comment :: Text
  , track   :: Maybe Word8
  , genre   :: Text
  } deriving (Show, Eq)

instance BinaryGet ID3v1 where
  bget = isolate 128 getID3v1

getID3v1 :: Get ID3v1
getID3v1 = do
  expectGetEq (getByteString 3) "TAG" "Expected ID3v1 marker `TAG`"
  title  <- getTag 30
  artist <- getTag 30
  album  <- getTag 30
  year   <- T.filter isDigit <$> getTag 4
  when (T.length year == 0) (F.fail "Year must contain at least one digit")
  hasTrackNum <- isID3v1_1 <$> lookAhead (getByteString 30)
  comment     <- getTag (if hasTrackNum then 28 else 30)
  track       <- if hasTrackNum
    then do
      skip 1
      Just <$> getWord8
    else return Nothing
  genreIndex <- getWord8
  when (genreIndex > genericLength genres)
       (F.fail $ "Unknown genre index " ++ show genreIndex)
  return $ ID3v1
    { title
    , artist
    , album
    , year
    , comment
    , track
    , genre   = genres `genericIndex` genreIndex
    }

getTag :: Int -> Get Text
getTag n = do
  bs <- BS.takeWhile (/= 0) <$> getByteString n
  return $ decodeLatin1 bs

isID3v1_1 :: BS.ByteString -> Bool
isID3v1_1 bs = BS.index bs 28 == 0 && BS.index bs 29 /= 0

instance MetadataFormat ID3v1 where
  formatDesc = "ID3v1"

instance MetadataLocator ID3v1 where
  hLocate h = do
    hSeek h SeekFromEnd (-128)
    pos <- fromIntegral <$> hTell h
    tag <- BS.hGet h 3
    return $ case tag of
      "TAG" -> Just pos
      _ -> Nothing

instance TagReader ID3v1 where
  tags = undefined

id3v1Id :: BS.ByteString
id3v1Id = "TAG"
