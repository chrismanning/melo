{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Internal.Locate where


import           Data.Binary                              ( Get )
import           Data.Binary.Get                          ( runGetOrFail )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import           Data.Either
import           Data.List                     as List
import           System.IO

import           Melo.Format.Internal.Binary
import           Melo.Format.Internal.BinaryUtil
import           Melo.Format.Internal.Format

class (MetadataFormat a, BinaryGet a) => MetadataLocator a where
  locate :: L.ByteString -> Maybe Int
  locate = locateBinaryLazy @a
  hLocate :: Handle -> IO (Maybe Int)
  hLocate h = locate @a <$> hGetFileContents h

locateBinaryLazy :: forall a . BinaryGet a => L.ByteString -> Maybe Int
locateBinaryLazy bs = List.findIndex canGet (L.tails bs)
 where
  canGet :: L.ByteString -> Bool
  canGet bs' = isRight $ runGetOrFail (bget :: Get a) bs'

locateBinary :: forall a . BinaryGet a => BS.ByteString -> Maybe Int
locateBinary bs = locateBinaryLazy @a $ L.fromStrict bs
