{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Collection.Types where

import Data.Hashable
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import GHC.Generics
import Melo.Common.Uri
import Melo.Database.Repo
import Rel8
import Witch

data CollectionTable f = CollectionTable
  { id :: Column f CollectionRef,
    root_uri :: Column f Text,
    name :: Column f Text,
    watch :: Column f Bool,
    kind :: Column f Text
  }
  deriving (Generic, Rel8able)

type CollectionEntity = CollectionTable Result
deriving instance Show CollectionEntity

newtype CollectionRef = CollectionRef { unCollectionRef :: UUID}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)

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
    watch :: Bool
  }
  deriving (Show, Eq, Generic, GQLType)

instance From NewCollection (CollectionTable Expr) where
  from c@NewFilesystemCollection {name, watch} =
    CollectionTable
      { id = nullaryFunction "uuid_generate_v4",
        root_uri = lit $ T.pack $ show $ rootUri c,
        name = lit name,
        watch = lit watch,
        kind = lit $ kind c
      }
    where
      rootUri NewFilesystemCollection {rootPath} = fileUri $ T.unpack rootPath
      kind NewFilesystemCollection {} = "filesystem"

type UpdateCollection = CollectionEntity
