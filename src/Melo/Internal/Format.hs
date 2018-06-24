{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Format where

import Control.Monad.Fail as F
import Data.Binary (Get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List as List
import Data.Text as T
import System.IO
import Type.Reflection

import Melo.Internal.Binary

class MetadataFormat a where
  formatDesc :: String
  formatDesc' :: a -> String
  formatDesc' _ = formatDesc @ a

data FormatKind where
  ContainerKind :: [FormatKind] -> FormatKind
  MetadataKind :: MetadataReader a => TypeRep a -> FormatKind

locateMetadata :: FormatKind -> L.ByteString -> Maybe Int
locateMetadata (ContainerKind _) _ = Nothing
locateMetadata (MetadataKind ty) bs = loc ty bs where
  loc :: forall a. MetadataReader a => TypeRep a -> L.ByteString -> Maybe Int
  loc _ bs' = locate @ a bs'

class (MetadataFormat a, BinaryGet a) =>
      MetadataReader a
  where
  locate :: L.ByteString -> Maybe Int
  locate = locateBinaryLazy @a
  hLocate :: Handle -> IO (Maybe Int)
  hLocate h = locateBinaryLazy @a <$> L.hGetContents h
  tags :: a -> [(Text, Text)]

hGetMetadata :: forall a. MetadataReader a => Handle -> IO a
hGetMetadata h = do
  bs <-
    hLocate @a h >>= \case
      Nothing -> F.fail $ "Unable to locate " ++ formatDesc @a
      Just i -> do
        hSeek h AbsoluteSeek (fromIntegral i)
        L.hGetContents h
  return $ bdecode bs

locateBinaryLazy ::
     forall a. BinaryGet a
  => L.ByteString
  -> Maybe Int
locateBinaryLazy bs = List.findIndex canGet (L.tails bs)
  where
    canGet :: L.ByteString -> Bool
    canGet bs' = isRight $ runGetOrFail (bget :: Get a) bs'

locateBinary ::
     forall a. BinaryGet a
  => BS.ByteString
  -> Maybe Int
locateBinary bs = locateBinaryLazy @a $ L.fromStrict bs
