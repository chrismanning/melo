{-# LANGUAGE AllowAmbiguousTypes #-}
module Melo.Format.Detect
  ( DetectedP (..),
    Detected (..),
    Detector (..),
    detectPath,
    hDetect,
    detect,
    withDetected,
    SupportedFormats,
  )
where

import Control.Applicative
import Melo.Format.Flac (Flac)
import Melo.Format.Internal.Detect
import Melo.Format.Internal.Tag
import Melo.Format.OggVorbis (OggVorbis)
import Melo.Format.WavPack (WavPack)
import System.IO

type SupportedFormats = '[Flac, WavPack, OggVorbis]

class PathDetect (d :: [*]) where
  pdf :: FilePath -> Maybe DetectedP

instance PathDetect '[] where
  pdf _ = Nothing

instance (Detector d, PathDetect ds) => PathDetect (d ': ds) where
  pdf p = pathDetectFormat @d p <|> pdf @ds p

detectPath :: FilePath -> Maybe DetectedP
detectPath = pdf @SupportedFormats

class HandleDetect (d :: [*]) where
  hdf :: Handle -> IO (Maybe DetectedP)

instance HandleDetect '[] where
  hdf _ = return Nothing

instance (Detector d, HandleDetect ds) => HandleDetect (d ': ds) where
  hdf h = do
    fmt <- hDetectFormat @d h
    fmts <- hdf @ds h
    return $ fmt <|> fmts

hDetect :: Handle -> IO (Maybe DetectedP)
hDetect = hdf @SupportedFormats

detect :: FilePath -> IO (Maybe DetectedP)
detect p = case detectPath p of
  Just dp -> return $ Just dp
  Nothing -> withBinaryFile p ReadMode hDetect

withDetected :: DetectedP -> (forall a. TagReader a => Detected a -> r) -> r
withDetected (DetectedP d) f = f d
