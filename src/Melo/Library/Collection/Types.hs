{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Collection.Types where

import Data.Hashable
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import qualified Data.Text as T
import Data.UUID
import Melo.Common.Uri
import Melo.Database.Repo
import Rel8

data CollectionTable f = CollectionTable
  { id :: Column f CollectionRef,
    root_uri :: Column f Text,
    name :: Column f Text,
    watch :: Column f Bool,
    kind :: Column f Text,
    rescan :: Column f Bool
  }
  deriving (Generic, Rel8able)

type CollectionEntity = CollectionTable Result
deriving instance Show CollectionEntity
deriving via (FromGeneric CollectionEntity) instance TextShow CollectionEntity
deriving via (CustomJSON JSONOptions CollectionEntity) instance ToJSON CollectionEntity

newtype CollectionRef = CollectionRef { unCollectionRef :: UUID}
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, DBType, DBEq, FromJSON, Hashable, ToJSON)
  deriving TextShow via FromGeneric CollectionRef

instance GQLType CollectionRef where
  type KIND CollectionRef = SCALAR

instance EncodeScalar CollectionRef where
  encodeScalar (CollectionRef uuid) = M.String $ toText uuid

instance DecodeScalar CollectionRef where
  decodeScalar (M.String s) = case fromText s of
    Nothing -> Left "CollectionRef must be UUID"
    Just uuid -> Right $ CollectionRef uuid
  decodeScalar _ = Left "CollectionRef must be a String"

instance From CollectionRef UUID where
  from (CollectionRef uuid) = uuid

instance Entity (CollectionTable Result) where
  type NewEntity (CollectionTable Result) = NewCollection
  type PrimaryKey (CollectionTable Result) = CollectionRef
  primaryKey e = e.id

data NewCollection = NewFilesystemCollection
  { rootPath :: Text,
    name :: Text,
    watch :: Bool,
    rescan :: Bool
  }
  deriving (Show, Eq, Generic, GQLType)
  deriving TextShow via FromGeneric NewCollection
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions NewCollection

instance From NewCollection (CollectionTable Expr) where
  from c@NewFilesystemCollection {name, watch, rescan} =
    CollectionTable
      { id = function "uuid_generate_v4" (),
        root_uri = lit $ showt $ rootUri c,
        name = lit name,
        watch = lit watch,
        kind = lit $ kind c,
        rescan = lit rescan
      }
    where
      rootUri NewFilesystemCollection {rootPath} = fileUri $ T.unpack rootPath
      kind NewFilesystemCollection {} = "filesystem"

type UpdateCollection = CollectionEntity
