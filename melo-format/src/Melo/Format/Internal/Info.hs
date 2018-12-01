module Melo.Format.Internal.Info where

data Info = Info {
  sampleRate :: SampleRate
, channels :: Channels
, totalSamples :: Maybe Integer
, bitsPerSample :: Maybe Int
} deriving (Show, Eq)

class InfoReader a where
  info :: a -> Info

newtype SampleRate = SampleRate Integer
  deriving (Show, Eq)

data Channels = Mono | Stereo | JointStereo | MultiChannel ChannelMask
  deriving (Show, Eq)

data ChannelMask = ChannelMask
  deriving (Show, Eq)
