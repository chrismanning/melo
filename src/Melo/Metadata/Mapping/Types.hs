{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Types where

import Data.Aeson as A
  ( FromJSON (..),
    Options (..),
    ToJSON (..),
    Value (Array),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
  )
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Types
  ( prependFailure,
    typeMismatch,
  )
import Data.List.NonEmpty
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Text (Text)
import Data.Traversable
import Data.Vector qualified as V
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
  parseJSON (Array a) = FM.TagMapping . fromList . V.toList <$> for a (genericParseJSON jsonOptions)
  parseJSON invalid =
    prependFailure
      "parsing TagMapping failed, "
      (typeMismatch "Array" invalid)

instance ToJSON FM.TagMapping where
  toJSON (FM.TagMapping fms) = toJSON (toList fms)

instance FromJSON FM.FieldMappings where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.FieldMappings where
  toJSON = genericToJSON jsonOptions

instance FromJSON FM.FieldMapping where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.FieldMapping where
  toJSON = genericToJSON jsonOptions

instance FromJSON FM.FieldMatchMode where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON FM.FieldMatchMode where
  toJSON = genericToJSON jsonOptions

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
