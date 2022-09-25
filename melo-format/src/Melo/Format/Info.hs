module Melo.Format.Info
  ( Info (..),
    InfoReader (..),
    SampleRate (..),
    Channels (..),
    ChannelMask (..),
    samplesPerSecond,
    audioLengthMilliseconds,
    audioLength,
  )
where

import Data.Fixed
import Data.Functor
import Data.Time.Clock
import Melo.Format.Internal.Info

samplesPerSecond :: SampleRate -> Integer
samplesPerSecond (SampleRate r) = r

audioLength :: Info -> Maybe NominalDiffTime
audioLength i =
  let samples :: Maybe Pico = fromIntegral <$> totalSamples i
      samplesPerSec = fromIntegral (samplesPerSecond $ sampleRate i)
   in samples <&> (/ samplesPerSec) <&> secondsToNominalDiffTime

audioLengthMilliseconds :: Info -> Maybe Double
audioLengthMilliseconds i =
  let samples :: Maybe Double = fromIntegral <$> totalSamples i
      samplesPerSec = fromIntegral (samplesPerSecond $ sampleRate i)
   in samples <&> (/ samplesPerSec) <&> (* 1000)
