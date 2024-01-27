{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Collection.Types where

import Data.Hashable
import Data.Text qualified as T
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
    rescan :: Column f Bool,
    library :: Column f Bool
  }
  deriving (Generic, Rel8able)

type CollectionEntity = CollectionTable Result

deriving instance Show CollectionEntity

deriving via (FromGeneric CollectionEntity) instance TextShow CollectionEntity

deriving via (CustomJSON JSONOptions CollectionEntity) instance ToJSON CollectionEntity

newtype CollectionRef = CollectionRef {unCollectionRef :: UUID}
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, DBType, DBEq, FromJSON, Hashable, ToJSON)
  deriving (TextShow) via FromGeneric CollectionRef

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
    rescan :: Bool,
    library :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via FromGeneric NewCollection
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions NewCollection

instance From NewCollection (CollectionTable Expr) where
  from c@NewFilesystemCollection {name, watch, rescan, library} =
    CollectionTable
      { id = function "uuid_generate_v4" (),
        root_uri = lit $ showt $ rootUri c,
        name = lit name,
        watch = lit watch,
        kind = lit $ kind c,
        rescan = lit rescan,
        library = lit library
      }
    where
      rootUri NewFilesystemCollection {rootPath} = fileUri $ T.unpack rootPath
      kind NewFilesystemCollection {} = "filesystem"

data CollectionUpdates = CollectionUpdates
  { name :: Maybe Text,
    watch :: Maybe Bool,
    kind :: Maybe Text,
    rescan :: Maybe Bool,
    library :: Maybe Bool
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions CollectionUpdates
