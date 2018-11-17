module Melo.Info
  ( Info(..)
  , InfoReader(..)
  , InfoRead(..)
  , SampleRate(..)
  , Channels(..)
  , ChannelMask(..)
  , readInfo
  , readSampleRate
  , readChannels
  , readTotalSamples
  , readBitsPerSample
  , samplesPerSecond
  , hRunInfoReadM
  , readLengthMilliseconds
  , lengthMilliseconds
  , audioLength
  , readAudioLength
  )
where

import           Control.Exception
import           Control.Monad.Freer
import           Data.Fixed
import           Data.Functor
import           Data.Time.Clock
import           System.IO

import           Melo.Detect
import           Melo.Internal.Info
import           Melo.Metadata

data InfoRead a where
  ReadInfo :: InfoRead Info

readInfo :: Member InfoRead effs => Eff effs Info
readInfo = send ReadInfo

readSampleRate :: Member InfoRead effs => Eff effs SampleRate
readSampleRate = sampleRate <$> readInfo

readChannels :: Member InfoRead effs => Eff effs Channels
readChannels = channels <$> readInfo

readTotalSamples :: Member InfoRead effs => Eff effs (Maybe Integer)
readTotalSamples = totalSamples <$> readInfo

readLengthMilliseconds :: Member InfoRead effs => Eff effs (Maybe Double)
readLengthMilliseconds = lengthMilliseconds <$> readInfo

readAudioLength :: Member InfoRead effs => Eff effs (Maybe NominalDiffTime)
readAudioLength = audioLength <$> readInfo

readBitsPerSample :: Member InfoRead effs => Eff effs (Maybe Int)
readBitsPerSample = bitsPerSample <$> readInfo

samplesPerSecond :: SampleRate -> Integer
samplesPerSecond (SampleRate r) = r

audioLength :: Info -> Maybe NominalDiffTime
audioLength i =
  let samples :: Maybe Pico = fromIntegral <$> totalSamples i
      samplesPerSec         = fromIntegral (samplesPerSecond $ sampleRate i)
  in  samples <&> (/ samplesPerSec) <&> secondsToNominalDiffTime

lengthMilliseconds :: Info -> Maybe Double
lengthMilliseconds i =
  let samples :: Maybe Double = fromIntegral <$> totalSamples i
      samplesPerSec           = fromIntegral (samplesPerSecond $ sampleRate i)
  in  samples <&> (/ samplesPerSec) <&> (* 1000)

hRunInfoReadM
  :: forall effs a
   . LastMember IO effs
  => Handle
  -> Eff (InfoRead ': effs) a
  -> Eff effs a
hRunInfoReadM h a = send (hDetect h) >>= \case
  Nothing            -> throw UnknownFormat
  Just (DetectedP d) -> do
    let hReadMetadata' = getHReadMetadata d
    !i <- info <$> (send $ hReadMetadata' h)
    interpret
      (\case
        ReadInfo -> pure i
      )
      a
