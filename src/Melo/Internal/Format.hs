{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Format where

import Control.Monad.Fail as F
import Data.Binary
import Data.Binary.Get
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List as List
import Data.Text as T
import System.IO

class Binary a =>
      MetadataFormat a
  where
  formatId :: String
  locate :: L.ByteString -> Maybe Int
  locate = locateBinaryLazy @a
  hLocate :: Handle -> IO (Maybe Int)
  hLocate h = locateBinaryLazy @a <$> L.hGetContents h
  tags :: a -> [(Text, Text)]

hGetMetadata :: forall a. MetadataFormat a => Handle -> IO a
hGetMetadata h = do
  bs <-
    hLocate @a h >>= \case
      Nothing -> F.fail $ "Unable to locate " ++ formatId @a
      Just i -> do
        hSeek h AbsoluteSeek (fromIntegral i)
        L.hGetContents h
  return $ decode bs

locateBinaryLazy ::
     forall a. Binary a
  => L.ByteString
  -> Maybe Int
locateBinaryLazy bs = List.findIndex canGet (L.tails bs)
  where
    canGet :: L.ByteString -> Bool
    canGet bs' = isRight $ runGetOrFail getA bs'
    getA :: Get a
    getA = get

locateBinary ::
     forall a. Binary a
  => BS.ByteString
  -> Maybe Int
locateBinary bs = locateBinaryLazy @a $ L.fromStrict bs
