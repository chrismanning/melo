module Melo.Format.Riff where

import Control.Monad.Extra
import Data.Binary
import Data.Binary.Get
import Data.Coerce
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Vector qualified as V
import Melo.Format.Internal.BinaryUtil
import Melo.Format.Internal.Metadata
import Melo.Format.Internal.Tag
import Melo.Format.Mapping

riffId :: MetadataId
riffId = MetadataId "RIFF"

newtype RiffInfo = RiffInfo Tags

getRiffInfo :: Get RiffInfo
getRiffInfo = do
  expectGetEq (getByteString 4) "INFO" "Chunk doesn't contain INFO subchunk"
  tags <-
    loopM
      ( \pairs ->
          isEmpty >>= \case
            True -> pure $ Right pairs
            False -> Left . V.snoc pairs <$> getInfoPair
      )
      V.empty
  pure $ RiffInfo (Tags tags)

getInfoPair :: Get (Text, Text)
getInfoPair = do
  name <- getUtf8Text 4
  sz <- getWord32le
  bytes <- getByteString (fromIntegral sz - 1)
  skip 1
  let text = case decodeUtf8' bytes of
        Right text -> text
        Left _ -> decodeLatin1 bytes
  pure (name, text)

instance MetadataFormat RiffInfo where
  metadataFormat =
    MetadataFormatDesc
      { formatId = riffId,
        formatDesc = "RIFF LIST INFO"
      }
  fieldMappingSelector = riff
  readTags = coerce
  replaceWithTags _riff tags = RiffInfo tags
  metadataSize (RiffInfo (Tags tags)) = toInteger (V.foldl' (\acc (_k, v) -> acc + 8 + T.length v) 0 tags)
