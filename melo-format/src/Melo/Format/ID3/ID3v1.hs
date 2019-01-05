module Melo.Format.ID3.ID3v1
  ( ID3v1(..)
  , iD3v1Id
  , hReplaceID3v1
  , hRemoveID3v1
  )
where

import           Control.Monad
import           Control.Monad.Fail            as F
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Char8         as C8
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import           Data.Char
import           Data.Functor
import           Data.List                                ( genericIndex
                                                          , genericLength
                                                          , elemIndex
                                                          )
import           Data.Maybe
import           Data.Text                     as T
import           Data.Text.Encoding
import           System.IO

import           Melo.Format.ID3.ID3v1Genre
import           Melo.Format.Internal.BinaryUtil
import           Melo.Format.Internal.Format
import           Melo.Format.Internal.Locate
import           Melo.Format.Internal.Tag
import           Melo.Format.Mapping                      ( trackTitleTag
                                                          , trackArtistTag
                                                          , albumTitleTag
                                                          , yearTag
                                                          , commentTag
                                                          , trackNumberTag
                                                          , genreTag
                                                          , FieldMappings(id3v1)
                                                          , headTagMapping
                                                          , FieldMapping
                                                            ( toCanonicalForm
                                                            )
                                                          )

data ID3v1 = ID3v1
  { title   :: !Text
  , artist  :: !Text
  , album   :: !Text
  , year    :: !Text
  , comment :: !Text
  , track   :: !(Maybe Word8)
  , genre   :: !Text
  } deriving (Show, Eq)

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
  tags id3 = Tags $ catMaybes [
      tag trackTitleTag title
    , tag trackArtistTag artist
    , tag albumTitleTag album
    , tag yearTag year
    , tag commentTag comment
    , do
        fm <- headTagMapping trackNumberTag
        track' <- track id3
        pure (toCanonicalForm $ id3v1 fm, T.pack $ show track')
    , tag genreTag genre
    ]
    where
      tag tm sel = headTagMapping tm <&> \fm ->
        (toCanonicalForm $ id3v1 fm, sel id3)

instance Binary ID3v1 where
  get = isolate 128 getID3v1
  put = putID3v1

getID3v1 :: Get ID3v1
getID3v1 = do
  expectGetEq (getByteString 3) iD3v1Id "Expected ID3v1 marker `TAG`"
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
getTag n = decodeLatin1 . BS.takeWhile (/= 0) <$> getByteString n

isID3v1_1 :: BS.ByteString -> Bool
isID3v1_1 bs = BS.index bs 28 == 0 && BS.index bs 29 /= 0

iD3v1Id :: BS.ByteString
iD3v1Id = "TAG"

putID3v1 :: ID3v1 -> Put
putID3v1 id3 = do
  putByteString iD3v1Id
  putTag 30 (title id3)
  putTag 30 (artist id3)
  putTag 30 (album id3)
  putTag 4  (year id3)
  case track id3 of
    Just track' -> do
      putTag 28 (comment id3)
      putWord8 0
      putWord8 track'
    Nothing -> putTag 30 (comment id3)
  putWord8 genreIndex
  where genreIndex = maybe 0 fromIntegral $ elemIndex (genre id3) genres

putTag :: Int -> Text -> Put
putTag n t = putLazyByteString
  $ L.take (fromIntegral n) (L.fromStrict (encodeLatin1 t) <> L.repeat 0)
  where
    encodeLatin1 a = C8.pack $ T.unpack a

hReplaceID3v1 :: Handle -> ID3v1 -> IO ()
hReplaceID3v1 h id3 = do
  l <- hLocate @ID3v1 h
  case fromIntegral <$> l of
    Just l' -> hSeek h AbsoluteSeek l'
    Nothing -> do
      hSeek h SeekFromEnd 0
      hFileSize h <&> (+ 128) >>= hSetFileSize h
  let tagBuf = runPut $ put id3
  L.hPut h tagBuf

hRemoveID3v1 :: Handle -> IO ()
hRemoveID3v1 h = do
  l <- hLocate @ID3v1 h
  case fromIntegral <$> l of
    Just l' -> hSetFileSize h l'
    Nothing -> pure ()
