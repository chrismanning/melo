{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prelude
  ( module Prelude,
    module Control.Lens,
    module Data.Bifunctor,
    module Data.Foldable,
    module Data.Kind,
    module Deriving.Aeson,
    module GHC.Generics,
    module GHC.Records,
    module TextShow,
    module TextShow.Generic,
    module TextShow.Instances,
    module Witherable,
    From (..),
    NonEmpty (..),
    Text (),
    TryFrom (..),
    TryFromException (..),
    Vector (),
    fromMaybe,
    into,
    tryInto,
    AsFrame (..),
    AsMetadata (..),
    ToUpper (),
    ToLower (),
    StripSuffix (),
  )
where

import Control.Lens hiding (each, from, to)
import Country
import Data.Aeson as JSON
import Data.Bifunctor
import Data.Char qualified
import Data.Foldable
import Data.Hashable
import Data.Kind
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Data.Proxy
import Data.Text
import Data.Time.LocalTime
import Data.Traversable
import Data.Vector (Vector)
import Data.Vector qualified as V
import Deriving.Aeson
import GHC.Generics (Generic ())
import GHC.Records
import GHC.TypeLits
import Melo.Format qualified as F
import Network.RSocket qualified as RSocket
import Network.URI
import Rel8 (JSONBEncoded (..))
import TextShow hiding
  ( Builder,
    flush,
    fromLazyText,
    fromString,
    fromText,
    lengthB,
    singleton,
    toLazyText,
    toLazyTextWith,
    toString,
    toText,
    unlinesB,
    unwordsB,
  )
import TextShow.Generic
import TextShow.Instances
import Witch
import Witherable
import "base" Prelude hiding
  ( filter,
    head,
    tail,
  )

instance From (NonEmpty a) (Vector a) where
  from (a :| as) = V.fromList (a : as)

deriving instance (Show a) => Show (JSONBEncoded a)

deriving newtype instance (ToJSON a) => ToJSON (JSONBEncoded a)

deriving via (FromStringShow (JSONBEncoded a)) instance (Show a) => TextShow (JSONBEncoded a)

deriving via (FromStringShow F.Metadata) instance TextShow F.Metadata

instance ToJSON F.Metadata where
  toJSON m =
    JSON.object
      [ ("format_id", toJSON m.formatId),
        ("format_desc", toJSON m.formatDesc),
        ("tags", toJSON m.tags)
      ]
  toEncoding m =
    JSON.pairs
      ( ("format_id" JSON..= m.formatId)
          <> ("format_desc" JSON..= m.formatDesc)
          <> ("tags" JSON..= m.tags)
      )

deriving via (FromStringShow F.MetadataFileId) instance TextShow F.MetadataFileId

deriving newtype instance FromJSON F.MetadataFileId

deriving newtype instance ToJSON F.MetadataFileId

deriving via (FromStringShow F.Tags) instance TextShow F.Tags

instance ToJSON F.Tags where
  toJSON (F.Tags tags) = JSON.Array (fmap (\(k, v) -> JSON.object [("key", toJSON k), ("value", toJSON v)]) tags)

deriving via (FromStringShow F.TagMapping) instance TextShow F.TagMapping

deriving via (FromStringShow F.FieldMappings) instance TextShow F.FieldMappings

deriving via (FromStringShow F.FieldMapping) instance TextShow F.FieldMapping

deriving via (FromStringShow F.PictureType) instance TextShow F.PictureType

deriving via CustomJSON JSONOptions F.PictureType instance FromJSON F.PictureType

deriving via CustomJSON JSONOptions F.PictureType instance ToJSON F.PictureType

deriving via (FromStringShow F.EmbeddedPicture) instance TextShow F.EmbeddedPicture

deriving via (FromStringShow F.Info) instance TextShow F.Info

deriving via (FromStringShow URI) instance TextShow URI

deriving via (FromStringShow Country) instance TextShow Country

deriving via (FromStringShow CalendarDiffTime) instance TextShow CalendarDiffTime

deriving newtype instance FromJSON F.MetadataId

deriving newtype instance ToJSON F.MetadataId

deriving via (FromStringShow F.MetadataId) instance TextShow F.MetadataId

deriving newtype instance FromJSON F.TagMapping

deriving newtype instance ToJSON F.TagMapping

deriving via CustomJSON JSONOptions F.FieldMappings instance FromJSON F.FieldMappings

deriving via CustomJSON JSONOptions F.FieldMappings instance ToJSON F.FieldMappings

deriving via CustomJSON JSONOptions F.FieldMapping instance FromJSON F.FieldMapping

deriving via CustomJSON JSONOptions F.FieldMapping instance ToJSON F.FieldMapping

deriving via CustomJSON JSONOptions F.FieldMatchMode instance FromJSON F.FieldMatchMode

deriving via CustomJSON JSONOptions F.FieldMatchMode instance ToJSON F.FieldMatchMode

type JSONOptions = '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, ConstructorTagModifier '[CamelToSnake, ToLower], SumObjectWithSingleField]

type CamelToScreamingSnake = (CamelToSnake, ToUpper)

data ToUpper

instance StringModifier ToUpper where
  getStringModifier = fmap Data.Char.toUpper

data ToLower

instance StringModifier ToLower where
  getStringModifier = fmap Data.Char.toLower

data StripSuffix s

instance (KnownSymbol s) => StringModifier (StripSuffix s) where
  getStringModifier = stripSuffix (symbolVal (Proxy @s))
    where
      stripSuffix s x = if s `List.isSuffixOf` x then List.take (List.length x - List.length s) x else x

deriving newtype instance FromJSON RSocket.StreamId

deriving newtype instance ToJSON RSocket.StreamId

instance (Hashable a) => Hashable (Vector a) where
  hashWithSalt s v = hashWithSalt s (toList v)

makeClassyPrisms ''RSocket.Frame
makeClassyPrisms ''RSocket.Metadata
