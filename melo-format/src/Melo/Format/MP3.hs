module Melo.Format.MP3 where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Bits
import qualified Data.Binary.Bits.Get          as BG
import qualified Data.Binary.Bits.Put          as BP
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as L
import           Data.Foldable
import           Data.Int
import           Data.Text                                ( Text )
import           Data.Text.Encoding
import           System.IO

import           Melo.Format.Internal.Binary
import           Melo.Format.Internal.BinaryUtil
import           Melo.Format.Internal.Encoding
import           Melo.Format.Internal.Format
import           Melo.Format.Internal.Locate
import           Melo.Format.Internal.Tag

data MP3 = MP3 {
  bitrate :: BitRate
, sampleRate :: Integer
, channels :: Channels
}

data BitRate = VBR Integer | CBR Integer

data Channels = Stereo | JointStereo | DualChannel | Mono

data FrameHeader = FrameHeader {

} deriving (Show, Eq)

instance BinaryGet FrameHeader where
  bget = do
    header <- getWord32be
    undefined
