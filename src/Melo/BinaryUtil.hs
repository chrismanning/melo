module Melo.BinaryUtil (getUTF8Text) where

import Data.Binary.Get
import Data.Text
import Data.Text.Encoding

getUTF8Text :: Int -> Get Text
getUTF8Text n = do
    bs <- getByteString n
    case decodeUtf8' bs of
        Left e -> fail $ "Error decoding string: " ++ show e
        Right s -> return s
