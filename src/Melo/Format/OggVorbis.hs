module Melo.Format.OggVorbis where

import Melo.Format.Ogg
import Melo.Format.Vorbis

import Melo.Internal.Binary
import Melo.Internal.Format

data OggVorbis = OggVorbis Identification FramedVorbisComments
  deriving (Eq, Show)

instance BinaryGet OggVorbis where
  bget = do
    OggPage (IdentificationHeader ident) <- bget
    OggPage (CommentsHeader vc) <- bget
    return $ OggVorbis ident vc

instance MetadataFormat OggVorbis where
  formatDesc = "OggVorbis"

instance MetadataLocator OggVorbis

instance MetadataReader OggVorbis where
  tags = undefined
