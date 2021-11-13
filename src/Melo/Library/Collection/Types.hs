{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Melo.Library.Collection.Types where

import Data.Hashable
import Data.List.NonEmpty
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Data.UUID.V4
import GHC.Generics
import Melo.Common.FileSystem
import Melo.Common.Uri
import Melo.Database.Repo
import qualified Melo.Format as F
import Rel8
import System.IO.Unsafe
import Witch

data CollectionTable f = CollectionTable
  { id :: Column f CollectionRef,
    root_uri :: Column f Text,
    name :: Column f Text,
    watch :: Column f Bool,
    kind :: Column f Text
  }
  deriving (Generic, Rel8able)

type Collection = CollectionTable Result
deriving instance Show Collection

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

data NewCollection = NewFilesystemCollection
  { rootPath :: Text,
    name :: Text,
    watch :: Bool
  }
  deriving (Show, Eq, Generic)

instance GQLType NewCollection where
  type KIND NewCollection = INPUT

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

instance From NewCollection (CollectionTable Result) where
  from c@NewFilesystemCollection {name, watch} =
    CollectionTable
      { id = CollectionRef $ unsafeDupablePerformIO nextRandom,
        root_uri = T.pack $ show $ rootUri c,
        name = name,
        watch = watch,
        kind = kind c
      }
    where
      rootUri NewFilesystemCollection {rootPath} = fileUri $ T.unpack rootPath
      kind NewFilesystemCollection {} = "filesystem"

type UpdateCollection = CollectionTable Result

data SourceMoveError
  = FileSystemMoveError MoveError
  | PatternError
  | NoSuchSource
  | SourcePathError
  deriving (Show)

data SourcePathPattern
  = LiteralPattern FilePath
  | GroupPattern (NonEmpty SourcePathPattern)
  | MappingPattern F.TagMapping
  | DefaultPattern SourcePathPattern SourcePathPattern
  | PrintfPattern String SourcePathPattern
