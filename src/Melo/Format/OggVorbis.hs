module Melo.Format.OggVorbis where

import Type.Reflection

import Melo.Format.Ogg
import Melo.Format.Vorbis

import Melo.Internal.Binary
import Melo.Internal.Format

data OggVorbis = OggVorbis Identification FramedVorbisComments
  deriving (Eq, Show, Typeable)

instance BinaryGet OggVorbis where
  bget = do
    OggPage (IdentificationHeader ident) <- bget
    OggPage (CommentsHeader vc) <- bget
    return $ OggVorbis ident vc

instance MetadataFormat OggVorbis where
  formatDesc = "OggVorbis"
  formatKind = ContainerKind [MetadataKind $ (typeRep @ OggVorbis)]

instance MetadataReader OggVorbis where
  tags = undefined
