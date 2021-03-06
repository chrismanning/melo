{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Internal.Locate where

import Control.Monad.Fail as F
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List as List
import Melo.Format.Internal.Binary
import Melo.Format.Internal.BinaryUtil
import Numeric.Natural
import System.IO

class BinaryGet a => MetadataLocator a where
  locate :: L.ByteString -> Maybe Int
  locate = locateBinaryLazy @a

  hLocate :: Handle -> IO (Maybe Natural)
  hLocate h = fmap fromIntegral . locate @a <$> hGetFileContents h

locateBinaryLazy :: forall a. BinaryGet a => L.ByteString -> Maybe Int
locateBinaryLazy bs = List.findIndex canGet (L.tails bs)
  where
    canGet :: L.ByteString -> Bool
    canGet bs' = isRight $ runGetOrFail (bget @a) bs'

locateBinary :: forall a. BinaryGet a => BS.ByteString -> Maybe Int
locateBinary bs = locateBinaryLazy @a $ L.fromStrict bs

hLocateGet :: forall a. (MetadataLocator a) => Handle -> IO a
hLocateGet h = do
  bs <-
    hLocate @a h >>= \case
      Nothing -> F.fail "Could not locate metadata"
      Just i -> do
        hSeek h AbsoluteSeek (fromIntegral i)
        hGetFileContents h
  bdecodeOrThrowIO bs

hLocateGet' :: forall a. (MetadataLocator a) => Handle -> IO (Maybe a)
hLocateGet' h =
  hLocate @a h >>= \case
    Nothing -> pure Nothing
    Just !i -> do
      hSeek h AbsoluteSeek (fromIntegral i)
      bs <- hGetFileContents h
      case bdecodeOrFail bs of
        Left _ -> pure Nothing
        Right (_, _, a) -> pure (Just a)
