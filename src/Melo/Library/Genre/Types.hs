{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Genre.Types where

import Data.Text
import Data.UUID
import GHC.Generics hiding (from)
import Melo.Database.Repo
import Rel8 (
  Column,
  DBEq,
  DBType,
  Expr,
  Rel8able,
  Result,
  nullaryFunction,
  lit,
  )
import Witch

data GenreTable f = GenreTable
  { id :: Column f GenreRef,
    name :: Column f Text,
    description :: Column f (Maybe Text)
  }
  deriving (Generic, Rel8able)

type Genre = GenreTable Result

newtype GenreRef = GenreRef UUID
  deriving (Show, Eq, Ord)
  deriving newtype (DBType, DBEq)

instance From GenreRef UUID where
  from (GenreRef uuid) = uuid 

instance Entity (GenreTable Result) where
  type NewEntity (GenreTable Result) = NewGenre
  type PrimaryKey (GenreTable Result) = GenreRef
  primaryKey e = e.id

data NewGenre = NewGenre
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Eq, Show)

instance From NewGenre (GenreTable Expr) where
  from NewGenre { name, description } = GenreTable {
    id = nullaryFunction "uuid_generate_v4",
    name = lit name,
    description = lit description
  }
