module Melo.Format.Internal.Encoding
  ( decodeUtf8OrFail,
    decodeUtf16BEOrFail,
    decodeUtf16WithBOMOrFail,
    encodeLatin1,
    utf16BeBom,
    utf16LeBom,
  )
where

import Control.Exception.Safe
import Control.Monad.Fail as Fail
import Data.ByteString as BS
import Data.Char (isLatin1)
import Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import GHC.IO.Unsafe

decodeUtf8OrFail :: MonadFail m => ByteString -> m Text
decodeUtf8OrFail = decodeUtfOrFail . decodeUtf8'

decodeUtf16BEOrFail :: MonadFail m => ByteString -> m Text
decodeUtf16BEOrFail = decodeUtfOrFail . decodeUtf' decodeUtf16BEWith

decodeUtf16LEOrFail :: MonadFail m => ByteString -> m Text
decodeUtf16LEOrFail = decodeUtfOrFail . decodeUtf' decodeUtf16LEWith

decodeUtf16WithBOMOrFail :: MonadFail m => ByteString -> m Text
decodeUtf16WithBOMOrFail bs =
  let bom = BS.take 2 bs
   in case bom of
        "\xFF\xFE" -> decodeUtf16LEOrFail (BS.drop 2 bs)
        "\xFE\xFF" -> decodeUtf16BEOrFail (BS.drop 2 bs)
        _noBom -> decodeUtf16BEOrFail bs

decodeUtfOrFail :: MonadFail m => Either UnicodeException Text -> m Text
decodeUtfOrFail = \case
  Left e -> Fail.fail $ "Error decoding string: " ++ show e
  Right s -> return s

decodeUtf' ::
  (OnDecodeError -> ByteString -> Text) ->
  ByteString ->
  Either UnicodeException Text
decodeUtf' f = unsafeDupablePerformIO . tryDeep . pure . f strictDecode

encodeLatin1 :: Text -> ByteString
encodeLatin1 = encodeUtf8 . T.filter isLatin1

utf16BeBom :: ByteString
utf16BeBom = "\xFE\xFF"

utf16LeBom :: ByteString
utf16LeBom = "\xFF\xFE"
