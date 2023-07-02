{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Format.Internal.Metadata where

import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Text
import GHC.Generics hiding (to)
import Data.Vector (Vector, (!?))
import GHC.Records
import Lens.Micro
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
    mappingSelector :: !FieldMappingSelector
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

type MetadataTagLens = Lens' Metadata (Vector Text)

tagLens :: TagMapping -> MetadataTagLens
tagLens mapping f metadata = let Metadata {tags, mappingSelector=sel} = metadata in
  mappedTag sel mapping f tags <&> \tags' -> metadata { tags = tags' }

instance HasField "tag" Metadata (TagMapping -> Vector Text) where
  getField metadata mapping = metadata ^. tagLens mapping

instance HasField "tagHead" Metadata (TagMapping -> Maybe Text) where
  getField metadata mapping = metadata ^. tagLens mapping . to (!? 0)

class MetadataFormat a where
  metadataFormat :: MetadataFormatDesc
  fieldMappingSelector :: FieldMappingSelector
  readTags :: a -> Tags
  replaceWithTags :: a -> Tags -> a
  metadataSize :: a -> Integer

metadataFactory :: forall a. MetadataFormat a => Tags -> Metadata
metadataFactory tags =
  let MetadataFormatDesc {..} = metadataFormat @a
   in Metadata
        { formatId,
          formatDesc,
          tags,
          mappingSelector = fieldMappingSelector @a
        }

data MetadataFormatDesc = MetadataFormatDesc
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
      tags = readTags a
   in Metadata
        { formatId = fmt.formatId,
          formatDesc = fmt.formatDesc,
          mappingSelector = fieldMappingSelector @a,
          tags
        }
