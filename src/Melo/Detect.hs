{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Detect(Detected(..), Detector(..), detectFile, hDetect) where

import Control.Applicative
import System.IO

import Melo.Format.Flac
import Melo.Format.OggVorbis
import Melo.Format.WavPack
import Melo.Internal.Detect

detectFile :: FilePath -> Maybe (Detected FilePath)
detectFile p =
  fileDetectFormat @Flac p <|>
  fileDetectFormat @WavPack p <|>
  fileDetectFormat @OggVorbis p

hDetect :: Handle -> IO (Maybe (Detected Handle))
hDetect h = do
  flac <- hDetectFormat @Flac h
  wv <- hDetectFormat @WavPack h
  ogg <- hDetectFormat @OggVorbis h
  return $ flac <|> wv <|> ogg
