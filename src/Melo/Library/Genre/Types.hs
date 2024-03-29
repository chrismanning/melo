{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Genre.Types where

import Data.UUID
import Melo.Database.Repo
import Rel8 (
  Column,
  DBEq,
  DBType,
  Expr,
  Rel8able,
  Result,
  function,
  lit,
  )

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
  deriving TextShow via FromStringShow GenreRef

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
  deriving TextShow via FromGeneric NewGenre

instance From NewGenre (GenreTable Expr) where
  from NewGenre { name, description } = GenreTable {
    id = function "uuid_generate_v4" (),
    name = lit name,
    description = lit description
  }
