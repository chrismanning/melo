{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Detect(
  DetectedP(..)
, Detected(..)
, Detector(..)
, detectPath
, hDetect
, detect
, withDetected
) where

import Control.Applicative
import System.IO

import Melo.Format as Fmt
import Melo.Format.Flac
import Melo.Format.OggVorbis
import Melo.Format.WavPack
import Melo.Internal.Detect

detectPath :: FilePath -> Maybe DetectedP
detectPath p =
  pathDetectFormat @Flac p <|>
  pathDetectFormat @WavPack p <|>
  pathDetectFormat @OggVorbis p

hDetect :: Handle -> IO (Maybe DetectedP)
hDetect h = do
  flac <- hDetectFormat @Flac h
  wv <- hDetectFormat @WavPack h
  ogg <- hDetectFormat @OggVorbis h
  return $ flac <|> wv <|> ogg

detect :: FilePath -> IO (Maybe DetectedP)
detect p = case detectPath p of
  Just dp -> return $ Just dp
  Nothing -> withBinaryFile p ReadMode hDetect

withDetected :: DetectedP -> (forall a. MetadataReader a => Detected a -> r) -> r
withDetected (DetectedP d) f = f d
