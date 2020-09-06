{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}

module Melo.Format.Internal.Metadata where

import Data.Generics.Labels ()
import Data.HashMap.Strict
import Data.Hashable
import Data.Text
import GHC.Generics hiding (to)
import GHC.OverloadedLabels
import Lens.Micro hiding (lens)
import Melo.Format.Internal.Info
import Melo.Format.Internal.Tag
import Melo.Format.Mapping

data MetadataFileFactory m = MetadataFileFactory
  { priority :: !Int,
    fileId :: !MetadataFileId,
    readMetadataFile :: FilePath -> m MetadataFile,
    writeMetadataFile :: MetadataFile -> FilePath -> m (),
    detectFile :: FilePath -> m Bool
  }
  deriving (Generic)

newtype MetadataFileId = MetadataFileId Text
  deriving (Generic, Show)
  deriving newtype (Hashable, Eq, Ord)

data MetadataFile = MetadataFile
  { metadata :: !(HashMap MetadataId Metadata),
    audioInfo :: !Info,
    fileId :: !MetadataFileId,
    filePath :: !FilePath
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
    showString "Metadata {formatId = " . showsPrec d formatId
      . showString ", formatDesc = "
      . showsPrec d formatDesc
      . showString ", tags = "
      . showsPrec d tags
      . showString "}"

instance IsLabel "formatId" (Getting MetadataId Metadata MetadataId) where
  fromLabel = to (formatId :: Metadata -> MetadataId)

instance IsLabel "formatDesc" (Getting Text Metadata Text) where
  fromLabel = to (formatDesc :: Metadata -> Text)

instance IsLabel "tags" (Getting Tags Metadata Tags) where
  fromLabel = to (tags :: Metadata -> Tags)

instance Eq Metadata where
  a == b =
    (a ^. #formatId :: MetadataId) == b ^. #formatId
      && (a ^. #formatDesc :: Text) == b ^. #formatDesc
      && (a ^. #tags :: Tags) == b ^. #tags

class MetadataFormat a where
  metadataFormat :: MetadataFormatDesc
  metadataLens :: TagMapping -> TagLens
  metadataFactory :: MetadataId -> Tags -> Maybe Metadata
  default metadataFactory :: MetadataId -> Tags -> Maybe Metadata
  metadataFactory mid tags =
    let MetadataFormat {..} = metadataFormat @a
     in if mid == formatId
          then
            Just
              Metadata
                { formatId,
                  formatDesc,
                  tags,
                  lens = metadataLens @a
                }
          else Nothing

data MetadataFormatDesc = MetadataFormat
  { formatId :: !MetadataId,
    formatDesc :: !Text
  }
  deriving (Generic, Hashable)

extractMetadata :: forall a. (TagReader a, MetadataFormat a) => a -> Metadata
extractMetadata a =
  Metadata
    { formatId = metadataFormat @a ^. #formatId,
      formatDesc = metadataFormat @a ^. #formatDesc,
      tags = readTags a,
      lens = metadataLens @a
    }
