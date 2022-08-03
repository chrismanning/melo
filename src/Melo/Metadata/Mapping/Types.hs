{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Types where

import Data.Aeson as A
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    defaultOptions,
    genericParseJSON,
    genericToEncoding,
  )
import Data.Aeson.Casing (snakeCase)
import Data.List.NonEmpty
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Text (Text)
import GHC.Generics
import GHC.Records
import Melo.Database.Repo
import Melo.Format qualified as F
import Melo.Format.Mapping qualified as FM
import Rel8
import Witch

data TagMappingTable f = TagMappingTable
  { name :: Column f Text,
    field_mappings :: Column f (JSONBEncoded FM.TagMapping)
  }
  deriving (Generic, Rel8able)

type TagMappingEntity = TagMappingTable Result

deriving instance Show TagMappingEntity

deriving newtype instance Show (JSONBEncoded FM.TagMapping)

instance FromJSON FM.TagMapping where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.TagMapping where
  toEncoding (FM.TagMapping fms) = genericToEncoding jsonOptions fms

instance FromJSON FM.FieldMappings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.FieldMappings where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON FM.FieldMapping where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.FieldMapping where
  toEncoding = genericToEncoding jsonOptions

instance FromJSON FM.FieldMatchMode where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.FieldMatchMode where
  toEncoding = genericToEncoding jsonOptions

instance HasField "tagMapping" TagMappingEntity FM.TagMapping where
  getField e = fromJSONBEncoded e.field_mappings

jsonOptions :: A.Options
jsonOptions =
  A.defaultOptions
    { A.fieldLabelModifier = snakeCase
    }

instance Entity (TagMappingTable Result) where
  type NewEntity (TagMappingTable Result) = NewTagMapping
  type PrimaryKey (TagMappingTable Result) = Text
  primaryKey e = e.name

data NewTagMapping = NewTagMapping
  { name :: Text,
    fieldMappings :: NonEmpty FM.FieldMappings
  }
  deriving (Show, Eq, Generic)

instance GQLType NewTagMapping where
  type KIND NewTagMapping = INPUT

instance From NewTagMapping (TagMappingTable Expr) where
  from c =
    TagMappingTable
      { name = lit c.name,
        field_mappings = lit (JSONBEncoded (FM.TagMapping c.fieldMappings))
      }

instance From NewTagMapping (TagMappingTable Result) where
  from c =
    TagMappingTable
      { name = c.name,
        field_mappings = JSONBEncoded (FM.TagMapping c.fieldMappings)
      }

type UpdateTagMapping = TagMappingTable Result
