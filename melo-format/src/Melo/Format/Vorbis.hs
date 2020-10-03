module Melo.Format.Vorbis where

import Control.Monad
import Control.Monad.Fail as F
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T (drop, findIndex, length, take)
import Data.Text.Encoding
import Data.Vector (Vector)
import qualified Data.Vector as V
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping

data Header
  = IdentificationHeader !Identification
  | CommentsHeader !FramedVorbisComments
  deriving (Eq, Show)

instance Binary Header where
  get = do
    t <- getPacketType
    expectGetEq (getByteString 6) "vorbis" "Expected vorbis identifier string"
    case t of
      Just IdentificationHeaderType -> IdentificationHeader <$> get
      Just CommentsHeaderType -> CommentsHeader <$> get
      _ -> F.fail "Unexpected packet header"
  put (IdentificationHeader i) = do
    putPacketType IdentificationHeaderType
    put i
  put (CommentsHeader vc) = do
    putPacketType CommentsHeaderType
    put vc

data PacketType
  = IdentificationHeaderType
  | CommentsHeaderType
  deriving (Eq, Show)

getPacketType :: Get (Maybe PacketType)
getPacketType =
  getWord8 <&> \case
    1 -> Just IdentificationHeaderType
    3 -> Just CommentsHeaderType
    _ -> Nothing

putPacketType :: PacketType -> Put
putPacketType IdentificationHeaderType = putWord8 1
putPacketType CommentsHeaderType = putWord8 3

data Identification = Identification
  { vorbisVersion :: !Word32,
    channels :: !Word8,
    sampleRate :: !Word32,
    bitrateMax :: !(Maybe Int32),
    bitrateNominal :: !(Maybe Int32),
    bitrateMin :: !(Maybe Int32),
    blockSize :: !Word8
  }
  deriving (Eq, Show)

instance Binary Identification where
  get = do
    vorbisVersion <- getWord32le
    channels <- getWord8
    sampleRate <- getWord32le
    bitrateMax <- getInt32le
    bitrateNominal <- getInt32le
    bitrateMin <- getInt32le
    blockSize <- getWord8
    expectGetEq getWord8 1 "Expected vorbis framing bit"
    return
      Identification
        { vorbisVersion,
          channels,
          sampleRate,
          bitrateMax = mfilter (> 0) $ Just bitrateMax,
          bitrateNominal = mfilter (> 0) $ Just bitrateNominal,
          bitrateMin = mfilter (> 0) $ Just bitrateMin,
          blockSize
        }
  put Identification {..} = do
    putWord32le vorbisVersion
    putWord8 channels
    putWord32le sampleRate
    putInt32le $ fromMaybe 0 bitrateMax
    putInt32le $ fromMaybe 0 bitrateNominal
    putInt32le $ fromMaybe 0 bitrateMin
    putWord8 blockSize
    putWord8 1

newtype FramedVorbisComments
  = FramedVorbisComments VorbisComments
  deriving (Show, Eq)

instance Binary FramedVorbisComments where
  get = do
    vc <- get
    expectGetEq getWord8 1 "Expected vorbis framing bit"
    return $ FramedVorbisComments vc
  put (FramedVorbisComments vc) = do
    putWord8 1
    put vc

data VorbisComments = VorbisComments
  { vendorString :: !Text,
    userComments :: !(Vector UserComment)
  }
  deriving (Show, Eq)

instance Binary VorbisComments where
  get = do
    vendorString <- getUtf8Text =<< fromIntegral <$> getWord32le
    numComments <- fromIntegral <$> getWord32le
    VorbisComments vendorString <$> V.replicateM numComments get
  put VorbisComments {..} = do
    let vs = encodeUtf8 vendorString
    putWord32le $ fromIntegral $ BS.length vs
    putByteString vs
    putWord32le $ fromIntegral $ V.length userComments
    mapM_ put userComments

data UserComment = UserComment Text Text
  deriving (Show, Eq)

instance Binary UserComment where
  get = do
    comment <- getUtf8Text =<< fromIntegral <$> getWord32le
    (name, value) <- case splitOnce (== '=') comment of
      Just x -> pure x
      Nothing -> F.fail "Invalid vorbis user comment"
    return $ UserComment name value
  put (UserComment k v) = do
    let comment = encodeUtf8 (k <> "=" <> v)
    putWord32le $ fromIntegral $ BS.length comment
    putByteString comment

splitOnce :: (Char -> Bool) -> Text -> Maybe (Text, Text)
splitOnce p t = do
  n <- T.findIndex p t
  pure (T.take n t, T.drop (min (T.length t) (n + 1)) t)

getVorbisTags :: VorbisComments -> Tags
getVorbisTags (VorbisComments _ cs) =
  Tags $ fmap (\(UserComment k v) -> (k, v)) cs

toUserComments :: Tags -> Vector UserComment
toUserComments (Tags ts) = fmap (uncurry UserComment) ts

replaceUserComments :: VorbisComments -> Vector UserComment -> VorbisComments
replaceUserComments (VorbisComments ven _) = VorbisComments ven

vorbisCommentsId :: MetadataId
vorbisCommentsId = MetadataId "VorbisComments"

instance MetadataFormat VorbisComments where
  metadataFormat =
    MetadataFormat
      { formatId = vorbisCommentsId,
        formatDesc = "Vorbis Comments"
      }
  metadataLens = vorbisTag
  readTags = getVorbisTags
  replaceWithTags vc tags = replaceUserComments vc (toUserComments tags)
  metadataSize = toInteger . L.length . runPut . put

vorbisTag :: TagMapping -> TagLens
vorbisTag = mappedTag vorbis
