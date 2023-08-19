module Melo.Metadata.Types where

import Melo.Format qualified as F

data MetadataFormatFactory = MetadataFormatFactory
  { metadataFormat :: F.MetadataFormatDesc
  , fieldMappingSelector :: F.FieldMappingSelector
  }
