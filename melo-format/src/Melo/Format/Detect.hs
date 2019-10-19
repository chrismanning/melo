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
import Data.Proxy
import Melo.Format.Flac (Flac)
import Melo.Format.Internal.Detect
import Melo.Format.Internal.Tag
import Melo.Format.OggVorbis (OggVorbis)
import Melo.Format.WavPack (WavPack)
import System.IO

type SupportedFormats = '[Flac, WavPack, OggVorbis]

class PathDetect (d :: [*]) where
  pdf :: Proxy d -> FilePath -> Maybe DetectedP

instance PathDetect '[] where
  pdf _ _ = Nothing

instance (Detector d, PathDetect ds) => PathDetect (d ': ds) where
  pdf _ p = pathDetectFormat @d p <|> pdf (Proxy @ds) p

detectPath :: FilePath -> Maybe DetectedP
detectPath = pdf (Proxy @SupportedFormats)

class HandleDetect (d :: [*]) where
  hdf :: Proxy d -> Handle -> IO (Maybe DetectedP)

instance HandleDetect '[] where
  hdf _ _ = return Nothing

instance (Detector d, HandleDetect ds) => HandleDetect (d ': ds) where
  hdf _ h = do
    fmt <- hDetectFormat @d h
    fmts <- hdf (Proxy @ds) h
    return $ fmt <|> fmts

hDetect :: Handle -> IO (Maybe DetectedP)
hDetect = hdf (Proxy @SupportedFormats)

detect :: FilePath -> IO (Maybe DetectedP)
detect p = case detectPath p of
  Just dp -> return $ Just dp
  Nothing -> withBinaryFile p ReadMode hDetect

withDetected :: DetectedP -> (forall a. TagReader a => Detected a -> r) -> r
withDetected (DetectedP d) f = f d
