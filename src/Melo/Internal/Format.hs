{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Internal.Format where

import Control.Monad
import Control.Monad.Fail as F
import Data.Binary (Get)
import Data.Binary.Get (runGetOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List as List
import Data.Text as T
import System.IO

import Debug.Trace

import Melo.Internal.Binary

newtype Tags = Tags [(Text, Text)]
  deriving (Show, Eq)

class MetadataFormat a where
  formatDesc :: String
  formatDesc' :: a -> String
  formatDesc' _ = formatDesc @ a

class (MetadataFormat a, BinaryGet a) => MetadataLocator a where
  locate :: L.ByteString -> Maybe Int
  locate = locateBinaryLazy @a
  hLocate :: Handle -> IO (Maybe Int)
  hLocate h = locate @a <$> L.hGetContents h

class MetadataFormat a =>
      MetadataReader a
  where
  tags :: a -> Tags

hGetMetadata :: forall a. MetadataLocator a => Handle -> IO a
hGetMetadata h = do
  bs <-
    hLocate @a h >>= \case
      Nothing -> F.fail $ "Unable to locate " ++ formatDesc @a
      Just i -> do
        traceIO $ "Found " ++ formatDesc @a ++ " at " ++ show i
        hIsClosed h >>= \c -> traceIO $ "h is " ++ if c then "closed" else "open"
        hSeek h AbsoluteSeek (fromIntegral i)
        hTell h >>= \p -> traceIO $ "h is at " ++ show p
        L.hGetContents h
  when (L.null bs) $ traceIO "no bytes"
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
