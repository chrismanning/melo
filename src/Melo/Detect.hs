{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Detect
  ( DetectedP(..)
  , Detected(..)
  , Detector(..)
  , detectPath
  , hDetect
  , detect
  , withDetected
  , SupportedFormats
  )
where

import           Control.Applicative
import           Data.Proxy
import           System.IO

import           Melo.Format.Flac                         ( Flac )
import           Melo.Format.OggVorbis                    ( OggVorbis )
import           Melo.Format.WavPack                      ( WavPack )
import           Melo.Internal.Detect
import           Melo.Internal.Tag
import           Melo.Internal.TypeLevel

type SupportedFormats = '[Flac, WavPack, OggVorbis]

class PathDetect (d :: [*]) where
  pdf :: FilePath -> Maybe DetectedP

instance PathDetect '[] where
  pdf _ = Nothing

instance (Detector d, PathDetect ds) => PathDetect (KProxy d ': ds) where
  pdf p = pathDetectFormat @d p <|> pdf @ds p

detectPath :: FilePath -> Maybe DetectedP
detectPath p = pdf @(FMap KProxy SupportedFormats) p

class HandleDetect (d :: [*]) where
  hdf :: Handle -> IO (Maybe DetectedP)

instance HandleDetect '[] where
  hdf _ = return Nothing

instance (Detector d, HandleDetect ds) => HandleDetect (KProxy d ': ds) where
  hdf h = do
    fmt <- hDetectFormat @d h
    fmts <- hdf @ds h
    return $ fmt <|> fmts

hDetect :: Handle -> IO (Maybe DetectedP)
hDetect h = hdf @(FMap KProxy SupportedFormats) h

detect :: FilePath -> IO (Maybe DetectedP)
detect p = case detectPath p of
  Just dp -> return $ Just dp
  Nothing -> withBinaryFile p ReadMode hDetect

withDetected :: DetectedP -> (forall a . TagReader a => Detected a -> r) -> r
withDetected (DetectedP d) f = f d
