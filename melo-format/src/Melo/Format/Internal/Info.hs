module Melo.Format.Internal.Info where

import           GHC.Generics

data Info = Info {
  sampleRate :: !SampleRate
, channels :: !Channels
, totalSamples :: !(Maybe Integer)
, bitsPerSample :: !(Maybe Int)
} deriving (Show, Eq, Generic)

class InfoReader a where
  info :: a -> Info

newtype SampleRate = SampleRate Integer
  deriving (Show, Eq, Generic)

data Channels = Mono | Stereo | JointStereo | MultiChannel ChannelMask
  deriving (Show, Eq, Generic)

data ChannelMask = ChannelMask
  deriving (Show, Eq, Generic)
