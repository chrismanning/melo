module Melo.Format.Vorbis where

import Control.Monad
import Control.Monad.Fail as F
import Data.Binary.Get
import Data.Functor
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T (take, length, drop, findIndex)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping

data Header
  = IdentificationHeader !Identification
  | CommentsHeader !FramedVorbisComments
  deriving (Eq, Show)

instance BinaryGet Header where
  bget = do
    t <- getPacketType
    expectGetEq (getByteString 6) "vorbis" "Expected vorbis identifier string"
    case t of
      Just IdentificationHeaderType -> IdentificationHeader <$> bget
      Just CommentsHeaderType -> CommentsHeader <$> bget
      _ -> F.fail "Unexpected packet header"

data PacketType
  = IdentificationHeaderType
  | CommentsHeaderType
  deriving (Eq, Show)

getPacketType :: Get (Maybe PacketType)
getPacketType = getWord8 <&> \case
  1 -> Just IdentificationHeaderType
  3 -> Just CommentsHeaderType
  _ -> Nothing

data Identification
  = Identification
      { vorbisVersion :: !Word32,
        channels :: !Word8,
        sampleRate :: !Word32,
        bitrateMax :: !(Maybe Int32),
        bitrateNominal :: !(Maybe Int32),
        bitrateMin :: !(Maybe Int32)
      }
  deriving (Eq, Show)

instance BinaryGet Identification where
  bget = do
    vorbisVersion <- getWord32le
    channels <- getWord8
    sampleRate <- getWord32le
    bitrateMax <- getInt32le
    bitrateNominal <- getInt32le
    bitrateMin <- getInt32le
    _blockSize <- getWord8
    expectGetEq getWord8 1 "Expected vorbis framing bit"
    return Identification
      { vorbisVersion,
        channels,
        sampleRate,
        bitrateMax = mfilter (> 0) $ Just bitrateMax,
        bitrateNominal = mfilter (> 0) $ Just bitrateNominal,
        bitrateMin = mfilter (> 0) $ Just bitrateMin
      }

newtype FramedVorbisComments
  = FramedVorbisComments VorbisComments
  deriving (Show, Eq)

instance BinaryGet FramedVorbisComments where
  bget = do
    vc <- bget
    expectGetEq getWord8 1 "Expected vorbis framing bit"
    return $ FramedVorbisComments vc

data VorbisComments
  = VorbisComments
      { vendorString :: !Text,
        userComments :: !(Vector UserComment)
      }
  deriving (Show, Eq)

instance BinaryGet VorbisComments where
  bget = do
    vendorString <- getUTF8Text =<< fromIntegral <$> getWord32le
    numComments <- fromIntegral <$> getWord32le
    VorbisComments vendorString <$> V.replicateM numComments bget

data UserComment = UserComment Text Text
  deriving (Show, Eq)

instance BinaryGet UserComment where
  bget = do
    comment <- getUTF8Text =<< fromIntegral <$> getWord32le
    (name, value) <- case splitOnce (== '=') comment of
      Just x -> pure x
      Nothing -> F.fail "Invalid vorbis user comment"
    return $ UserComment name value

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
  metadataFormat _ = MetadataFormat {
    formatId = vorbisCommentsId,
    formatDesc = "Vorbis Comments"
  }
  metadataLens _ = vorbisTag

instance TagReader VorbisComments where
  readTags = getVorbisTags

vorbisTag :: TagMapping -> TagLens
vorbisTag = mappedTag vorbis
