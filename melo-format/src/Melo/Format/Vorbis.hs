module Melo.Format.Vorbis where

import           Control.Monad
import           Data.Binary                              ( Binary(..) )
import           Data.Binary.Get
import           Data.Functor
import           Data.Int
import           Data.Text                     as T
import           Data.Word

import           Melo.Format.Internal.Binary
import           Melo.Format.Internal.BinaryUtil
import           Melo.Format.Internal.Tag

data Header
  = IdentificationHeader Identification
  | CommentsHeader FramedVorbisComments
  deriving (Eq, Show)

instance BinaryGet Header where
  bget = do
    t <- getPacketType
    expectGetEq (getByteString 6) "vorbis" "Expected vorbis identifier string"
    case t of
      Just IdentificationHeaderType -> IdentificationHeader <$> bget
      Just CommentsHeaderType -> CommentsHeader <$> bget
      _ -> fail "Unexpected packet header"

data PacketType
  = IdentificationHeaderType
  | CommentsHeaderType
  deriving (Eq, Show)

getPacketType :: Get (Maybe PacketType)
getPacketType = getWord8 <&> \case
  1 -> Just IdentificationHeaderType
  3 -> Just CommentsHeaderType
  _ -> Nothing

data Identification = Identification
  { vorbisVersion :: Word32
  , channels :: Word8
  , sampleRate :: Word32
  , bitrateMax :: Maybe Int32
  , bitrateNominal :: Maybe Int32
  , bitrateMin :: Maybe Int32
  }
  deriving (Eq, Show)

instance BinaryGet Identification where
  bget =  do
    vorbisVersion <- getWord32le
    channels <- getWord8
    sampleRate <- getWord32le
    bitrateMax <- getInt32le
    bitrateNominal <- getInt32le
    bitrateMin <- getInt32le
    _blockSize <- getWord8
    expectGetEq getWord8 1 "Expected vorbis framing bit"
    return
      Identification
        { vorbisVersion
        , channels
        , sampleRate
        , bitrateMax = mfilter (> 0) $ Just bitrateMax
        , bitrateNominal = mfilter (> 0) $ Just bitrateNominal
        , bitrateMin = mfilter (> 0) $ Just bitrateMin
        }

newtype FramedVorbisComments =
  FramedVorbisComments VorbisComments
  deriving (Show, Eq)

instance Binary FramedVorbisComments where
  put = undefined
  get = do
    vc <- get
    expectGetEq getWord8 1 "Expected vorbis framing bit"
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
    VorbisComments vendorString <$> replicateM numComments bget

data UserComment = UserComment Text Text
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
  pure (T.take n t, T.drop (min (T.length t) (n + 1)) t)

getVorbisTags :: VorbisComments -> Tags
getVorbisTags (VorbisComments _ cs) =
  Tags $ fmap (\(UserComment k v) -> (k, v)) cs

toUserComments :: Tags -> [UserComment]
toUserComments (Tags ts) = fmap (uncurry UserComment) ts

replaceUserComments :: VorbisComments -> [UserComment] -> VorbisComments
replaceUserComments (VorbisComments ven _) = VorbisComments ven
