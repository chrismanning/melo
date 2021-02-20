module Melo.Library.Collection.Types where

import Basement.From
import Data.Hashable
import Data.List.NonEmpty
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID
import Database.Beam
import Database.Beam.Postgres as PgB
import Melo.Common.FileSystem
import Melo.Common.Uri
import qualified Melo.Database.Model as DB
import qualified Melo.Format as F

data NewCollection = NewFilesystemCollection
  { rootPath :: Text,
    name :: Text,
    watch :: Bool
  }
  deriving (Show, Eq, Generic)

instance GQLType NewCollection where
  type KIND NewCollection = INPUT

instance From NewCollection (DB.CollectionT (QExpr Postgres s)) where
  from c =
    DB.Collection
      { id = default_,
        root_uri = val_ $ T.pack $ show $ rootUri c,
        name = val_ $ name c,
        watch = val_ $ watch c,
        kind = kind c
      }
    where
      rootUri NewFilesystemCollection {rootPath} = fileUri $ T.unpack rootPath
      kind NewFilesystemCollection {} = "filesystem"

type UpdateCollection = DB.Collection

newtype CollectionRef = CollectionRef UUID
  deriving (Show, Eq, Ord, Generic)

instance Hashable CollectionRef

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
