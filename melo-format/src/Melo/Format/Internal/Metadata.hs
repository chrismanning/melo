{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}

module Melo.Format.Internal.Metadata where

import Data.ByteString
import Data.Generics.Labels ()
import Data.HashMap.Strict
import Data.Hashable
import Data.Text
import GHC.Generics hiding (to)
import Melo.Format.Internal.Info
import Melo.Format.Internal.Tag
import Melo.Format.Mapping

data MetadataFileFactory m = MetadataFileFactory
  { priority :: !Int,
    fileId :: !MetadataFileId,
    readMetadataFile :: !(FilePath -> m MetadataFile),
    writeMetadataFile :: !(MetadataFile -> FilePath -> m ()),
    detectFile :: !(FilePath -> m Bool)
  }
  deriving (Generic)

newtype MetadataFileId = MetadataFileId Text
  deriving (Generic, Show)
  deriving newtype (Hashable, Eq, Ord)

data MetadataFile = MetadataFile
  { metadata :: !(HashMap MetadataId Metadata),
    audioInfo :: !Info,
    fileId :: !MetadataFileId,
    filePath :: !FilePath,
    pictures :: ![(PictureType, EmbeddedPicture)]
  }
  deriving (Generic, Eq, Show)

newtype MetadataId = MetadataId
  { unMetadataId :: Text
  }
  deriving (Generic, Show)
  deriving newtype (Hashable, Eq, Ord)

data Metadata = Metadata
  { formatId :: !MetadataId,
    formatDesc :: !Text,
    tags :: !Tags,
    lens :: !(TagMapping -> TagLens)
  }

instance Show Metadata where
  showsPrec d Metadata {..} =
    showString "Metadata {formatId = "
      . showsPrec d formatId
      . showString ", formatDesc = "
      . showsPrec d formatDesc
      . showString ", tags = "
      . showsPrec d tags
      . showString "}"

instance Eq Metadata where
  a == b =
    a.formatId == b.formatId
      && a.formatDesc == b.formatDesc
      && a.tags == b.tags

class MetadataFormat a where
  metadataFormat :: MetadataFormatDesc
  metadataLens :: TagMapping -> TagLens
  readTags :: a -> Tags
  replaceWithTags :: a -> Tags -> a
  metadataSize :: a -> Integer

metadataFactory :: forall a. MetadataFormat a => Tags -> Metadata
metadataFactory tags =
  let MetadataFormat {..} = metadataFormat @a
   in Metadata
        { formatId,
          formatDesc,
          tags,
          lens = metadataLens @a
        }

data MetadataFormatDesc = MetadataFormat
  { formatId :: !MetadataId,
    formatDesc :: !Text
  }
  deriving (Generic, Eq)

instance Hashable MetadataFormatDesc

data PictureType
  = Other
  | Icon32
  | IconOther
  | FrontCover
  | BackCover
  | Leaflet
  | Media
  | LeadArtist
  | Artist
  | Conductor
  | Band
  | Composer
  | Lyricist
  | RecordingLocation
  | DuringRecording
  | DuringPerformance
  | ScreenCapture
  | BrightColouredFish
  | Illustration
  | ArtistLogo
  | PublisherLogo
  deriving (Generic, Show, Eq, Enum, Read)

data EmbeddedPicture = EmbeddedPicture
  { mimeType :: Text,
    pictureData :: ByteString
  }
  deriving (Generic, Show, Eq)

extractMetadata :: forall a. MetadataFormat a => a -> Metadata
extractMetadata a =
  let fmt = metadataFormat @a
   in Metadata
        { formatId = fmt.formatId,
          formatDesc = fmt.formatDesc,
          tags = readTags a,
          lens = metadataLens @a
        }
