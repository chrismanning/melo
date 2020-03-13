module Melo.Format.Info
  ( Info (..),
    InfoReader (..),
    InfoRead (..),
    SampleRate (..),
    Channels (..),
    ChannelMask (..),
    readInfo,
    readSampleRate,
    readChannels,
    readTotalSamples,
    readBitsPerSample,
    samplesPerSecond,
--    hRunInfoReadM,
    readLengthMilliseconds,
    lengthMilliseconds,
    audioLength,
    readAudioLength,
  )
where

import Control.Exception.Safe
import Data.Fixed
import Data.Functor
import Data.Kind
import Data.Time.Clock
import Melo.Format.Error
import Melo.Format.Internal.Info
import Polysemy
import System.IO

data InfoRead (m :: Type -> Type) a where
  ReadInfo :: InfoRead m Info

makeSem ''InfoRead

readSampleRate :: Member InfoRead effs => Sem effs SampleRate
readSampleRate = sampleRate <$> readInfo

readChannels :: Member InfoRead effs => Sem effs Channels
readChannels = channels <$> readInfo

readTotalSamples :: Member InfoRead effs => Sem effs (Maybe Integer)
readTotalSamples = totalSamples <$> readInfo

readLengthMilliseconds :: Member InfoRead effs => Sem effs (Maybe Double)
readLengthMilliseconds = lengthMilliseconds <$> readInfo

readAudioLength :: Member InfoRead effs => Sem effs (Maybe NominalDiffTime)
readAudioLength = audioLength <$> readInfo

readBitsPerSample :: Member InfoRead effs => Sem effs (Maybe Int)
readBitsPerSample = bitsPerSample <$> readInfo

samplesPerSecond :: SampleRate -> Integer
samplesPerSecond (SampleRate r) = r

audioLength :: Info -> Maybe NominalDiffTime
audioLength i =
  let samples :: Maybe Pico = fromIntegral <$> totalSamples i
      samplesPerSec = fromIntegral (samplesPerSecond $ sampleRate i)
   in samples <&> (/ samplesPerSec) <&> secondsToNominalDiffTime

lengthMilliseconds :: Info -> Maybe Double
lengthMilliseconds i =
  let samples :: Maybe Double = fromIntegral <$> totalSamples i
      samplesPerSec = fromIntegral (samplesPerSecond $ sampleRate i)
   in samples <&> (/ samplesPerSec) <&> (* 1000)

--hRunInfoReadM ::
--  forall r a.
--  Member (Embed IO) r =>
--  Handle ->
--  Sem (InfoRead ': r) a ->
--  Sem r a
--hRunInfoReadM h a = embed (hDetect h) >>= \case
--  Nothing -> throw UnknownFormat
--  Just (DetectedP d) -> do
--    let hReadMetadata' = getHReadMetadata d
--    !i <- info <$> embed (hReadMetadata' h)
--    interpret
--      ( \case
--          ReadInfo -> pure i
--      )
--      a
