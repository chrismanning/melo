{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Format where

import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List as List
import Data.Text as T
import System.IO

class Binary a => MetadataFormat a where
  locate :: L.ByteString -> Maybe Int
  locate = locateBinaryLazy @ a

  hLocate :: Handle -> IO (Maybe Int)
  hLocate h = locateBinaryLazy @ a <$> L.hGetContents h

  tags :: a -> [(Text, Text)]

locateBinaryLazy :: forall a. Binary a => L.ByteString -> Maybe Int
locateBinaryLazy bs = List.findIndex canGet (L.tails bs)
  where
    canGet :: L.ByteString -> Bool
    canGet bs' = isRight $ runGetOrFail getA bs'
    getA :: Get a
    getA = get

locateBinary :: forall a. Binary a => BS.ByteString -> Maybe Int
locateBinary bs = locateBinaryLazy @ a $ L.fromStrict bs
