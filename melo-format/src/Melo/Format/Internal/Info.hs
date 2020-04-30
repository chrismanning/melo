module Melo.Format.Internal.Info where

import Data.Text (Text)
import GHC.Generics

data Info = Info
  { sampleRate :: !SampleRate,
    channels :: !Channels,
    totalSamples :: !(Maybe Integer),
    bitsPerSample :: !(Maybe Int),
    quality :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

class InfoReader a where
  info :: a -> Info

newtype SampleRate = SampleRate Integer
  deriving (Show, Eq, Generic)

data Channels = Mono | Stereo | JointStereo | MultiChannel ChannelMask
  deriving (Show, Eq, Generic)

data ChannelMask = ChannelMask
  deriving (Show, Eq, Generic)
