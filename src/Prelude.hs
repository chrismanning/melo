{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prelude
  ( module Prelude,
    module Control.Lens,
    module Data.Bifunctor,
    module Data.Foldable,
    module TextShow.Generic,
    module Witherable,
    From (..),
    FromStringShow (..),
    NonEmpty (..),
    Text (),
    TextShow (..),
    TryFrom (..),
    TryFromException (..),
    Vector (),
    fromMaybe,
    into,
    tryInto,
  )
where

import Control.Lens hiding (from, to, each)
import Control.Monad.Primitive
import Control.Monad.Trans
import Country
import Data.Aeson as A
import Data.Aeson.Casing (snakeCase)
import Data.Aeson.Types
  ( prependFailure,
    typeMismatch,
  )
import Data.Bifunctor
import Data.Foldable
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text
import Data.Time.LocalTime
import Data.Traversable
import Data.Vector (Vector)
import Data.Vector qualified as V
import Melo.Format qualified as F
import Network.URI
import Rel8 (JSONBEncoded (..))
import TextShow (FromStringShow (..))
import TextShow.Generic
import TextShow.Instances
import Web.Scotty.Trans
import Witch
import Witherable
import "base" Prelude hiding
  ( head,
    tail,
    filter,
  )

instance From (NonEmpty a) (Vector a) where
  from (a :| as) = V.fromList (a : as)

instance (PrimMonad m, ScottyError a) => PrimMonad (ActionT a m) where
  type PrimState (ActionT a m) = PrimState m
  primitive = lift . primitive

deriving instance Show a => Show (JSONBEncoded a)

deriving via (FromStringShow (JSONBEncoded a)) instance Show a => TextShow (JSONBEncoded a)

deriving via (FromStringShow F.Metadata) instance TextShow F.Metadata

deriving via (FromStringShow F.MetadataFileId) instance TextShow F.MetadataFileId

deriving via (FromStringShow F.Tags) instance TextShow F.Tags

deriving via (FromStringShow F.TagMapping) instance TextShow F.TagMapping

deriving via (FromStringShow F.FieldMappings) instance TextShow F.FieldMappings

deriving via (FromStringShow F.FieldMapping) instance TextShow F.FieldMapping

deriving via (FromStringShow F.PictureType) instance TextShow F.PictureType

deriving via (FromStringShow F.EmbeddedPicture) instance TextShow F.EmbeddedPicture

deriving via (FromStringShow F.Info) instance TextShow F.Info

deriving via (FromStringShow URI) instance TextShow URI

deriving via (FromStringShow Country) instance TextShow Country

deriving via (FromStringShow CalendarDiffTime) instance TextShow CalendarDiffTime

deriving newtype instance FromJSON F.MetadataId

deriving newtype instance ToJSON F.MetadataId

instance FromJSON F.TagMapping where
  parseJSON (Array a) = F.TagMapping . NE.fromList . toList <$> for a (genericParseJSON snakeCakeJsonOptions)
  parseJSON invalid =
    prependFailure
      "parsing TagMapping failed, "
      (typeMismatch "Array" invalid)

instance ToJSON F.TagMapping where
  toJSON (F.TagMapping fms) = toJSON (toList fms)

instance FromJSON F.FieldMappings where
  parseJSON = genericParseJSON snakeCakeJsonOptions

instance ToJSON F.FieldMappings where
  toJSON = genericToJSON snakeCakeJsonOptions

instance FromJSON F.FieldMapping where
  parseJSON = genericParseJSON snakeCakeJsonOptions

instance ToJSON F.FieldMapping where
  toJSON = genericToJSON snakeCakeJsonOptions

instance FromJSON F.FieldMatchMode where
  parseJSON = genericParseJSON snakeCakeJsonOptions

instance ToJSON F.FieldMatchMode where
  toJSON = genericToJSON snakeCakeJsonOptions

snakeCakeJsonOptions :: A.Options
snakeCakeJsonOptions =
  A.defaultOptions
    { A.fieldLabelModifier = snakeCase
    }
