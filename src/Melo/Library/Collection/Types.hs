module Melo.Library.Collection.Types where

import Basement.From
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Postgres as PgB
import Melo.Common.Uri
import qualified Melo.Database.Model as DB

data NewCollection = NewFilesystemCollection
  { rootPath :: FilePath,
    name :: Text,
    watch :: Bool
  }
  deriving (Show, Eq)

instance From NewCollection (DB.CollectionT (QExpr Postgres s)) where
  from c = DB.Collection {
    id = default_,
    root_uri = val_ $ T.pack $ show $ rootUri c,
    name = val_ $ name c,
    watch = val_ $ watch c,
    kind = kind c
  }
    where
    rootUri NewFilesystemCollection{rootPath} = fileUri rootPath
    kind NewFilesystemCollection {} = "filesystem"

type UpdateCollection = DB.Collection
