module Melo.Format.ID3.ID3v1
  ( ID3v1 (..),
    iD3v1Id,
    hReplaceID3v1,
    hRemoveID3v1,
    id3v1Id,
    id3v1Tag,
    id3v1Size,
  )
where

import Control.Monad
import Control.Monad.Fail as F
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Functor
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import qualified Data.Vector as V
import GHC.Generics
import Melo.Format.ID3.ID3v1Genre
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Encoding
import Melo.Format.Internal.Locate
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping
  ( FieldMapping
      ( toCanonicalForm
      ),
    FieldMappings (id3v1),
    TagMapping (..),
    albumTitleTag,
    commentTag,
    genreTag,
    headTagMapping,
    trackArtistTag,
    trackNumberTag,
    trackTitleTag,
    yearTag,
  )
import System.IO
import Text.Read

data ID3v1 = ID3v1
  { title :: !Text,
    artist :: !Text,
    album :: !Text,
    year :: !Text,
    comment :: !Text,
    track :: !(Maybe Word8),
    genre :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance MetadataFormat ID3v1 where
  metadataFormat =
    MetadataFormat
      { formatId = id3v1Id,
        formatDesc = let (MetadataId k) = id3v1Id in k
      }
  metadataLens = mappedTag id3v1
  readTags id3 =
    Tags $
      V.fromList $
        catMaybes
          [ tag trackTitleTag title,
            tag trackArtistTag artist,
            tag albumTitleTag album,
            tag yearTag year,
            tag commentTag comment,
            do
              let fm = headTagMapping trackNumberTag
              track' <- track id3
              pure (toCanonicalForm $ id3v1 fm, T.pack $ show track'),
            do
              g <- genre id3
              tag genreTag (const g)
          ]
    where
      tag tm sel =
        let fm = headTagMapping tm
         in Just (toCanonicalForm $ id3v1 fm, sel id3)
  replaceWithTags id3 newTags =
    id3
      { title = saveTag trackTitleTag,
        artist = saveTag trackArtistTag,
        album = saveTag albumTitleTag,
        year = saveTag yearTag,
        comment = saveTag commentTag,
        track = readMaybe $ T.unpack $ saveTag trackNumberTag,
        genre = Just $ saveTag genreTag
      }
    where
      saveTag tm =
        let fm = headTagMapping tm
         in fromMaybe
              ""
              (listToMaybe $ lookupTag (toCanonicalForm $ id3v1 fm) newTags)
  metadataSize = const id3v1Size

id3v1Id :: MetadataId
id3v1Id = MetadataId "ID3v1"

id3v1Tag :: TagMapping -> TagLens
id3v1Tag = mappedTag id3v1

id3v1Size :: Integer
id3v1Size = 128

instance MetadataLocator ID3v1 where
  hLocate h = do
    hSeek h SeekFromEnd (- id3v1Size)
    pos <- fromIntegral <$> hTell h
    tag <- BS.hGet h 3
    return $ case tag of
      "TAG" -> Just pos
      _ -> Nothing

instance Binary ID3v1 where
  get = isolate (fromInteger id3v1Size) getID3v1

  put = putID3v1

getID3v1 :: Get ID3v1
getID3v1 = do
  expectGetEq (getByteString 3) iD3v1Id "Expected ID3v1 marker `TAG`"
  title <- getTag 30
  artist <- getTag 30
  album <- getTag 30
  year <- T.filter isDigit <$> getTag 4
  when (T.empty == year) (F.fail "Year must contain at least one digit")
  hasTrackNum <- isID3v1_1 <$> lookAhead (getByteString 30)
  comment <- getTag (if hasTrackNum then 28 else 30)
  track <-
    if hasTrackNum
      then do
        skip 1
        Just <$> getWord8
      else return Nothing
  genreIndex <- getWord8
  return $
    ID3v1
      { title,
        artist,
        album,
        year,
        comment,
        track,
        genre = genres V.!? fromIntegral genreIndex
      }

getTag :: Int -> Get Text
getTag n = decodeLatin1 . BS.takeWhile (/= 0) <$> getByteString n

isID3v1_1 :: BS.ByteString -> Bool
isID3v1_1 bs = BS.index bs 28 == 0 && BS.index bs 29 /= 0

iD3v1Id :: BS.ByteString
iD3v1Id = "TAG"

putID3v1 :: ID3v1 -> Put
putID3v1 ID3v1 {..} = do
  putByteString iD3v1Id
  putTag 30 title
  putTag 30 artist
  putTag 30 album
  putTag 4 year
  case track of
    Just track' -> do
      putTag 28 comment
      putWord8 0
      putWord8 track'
    Nothing -> putTag 30 comment
  putWord8 genreIndex
  where
    genreIndex = case genre of
      Just genre' -> maybe 0xFF fromIntegral $ V.elemIndex genre' genres
      Nothing -> 0xFF

putTag :: Int -> Text -> Put
putTag n t =
  putLazyByteString $
    L.take (fromIntegral n) (L.fromStrict (encodeLatin1 t) <> L.repeat 0)

hReplaceID3v1 :: Handle -> ID3v1 -> IO ()
hReplaceID3v1 h id3 = do
  l <- hLocate @ID3v1 h
  case fromIntegral <$> l of
    Just l' -> hSeek h AbsoluteSeek l'
    Nothing -> do
      hSeek h SeekFromEnd 0
      hFileSize h <&> (+ id3v1Size) >>= hSetFileSize h
  let tagBuf = runPut $ put id3
  L.hPut h tagBuf

hRemoveID3v1 :: Handle -> IO ()
hRemoveID3v1 h = do
  l <- hLocate @ID3v1 h
  case fromIntegral <$> l of
    Just l' -> hSetFileSize h l'
    Nothing -> pure ()
