{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Genre.Types where

import Data.Hashable
import Data.UUID
import Data.Vector qualified as V
import Melo.Database.Repo
import Rel8 (
  Column,
  DBEq,
  DBType,
  Expr,
  JSONBEncoded (..),
  Rel8able,
  Result,
  function,
  lit,
  )

newtype GenreRef = GenreRef UUID
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, DBType, DBEq, FromJSON, Hashable, ToJSON)
  deriving TextShow via FromGeneric GenreRef

instance From GenreRef UUID where
  from (GenreRef uuid) = uuid

data GenreTable f = GenreTable
  { id :: Column f GenreRef,
    name :: Column f Text,
    description :: Column f (Maybe Text),
    top_level :: Column f Bool
  }
  deriving (Generic, Rel8able)

type GenreEntity = GenreTable Result
deriving via (FromGeneric GenreEntity) instance TextShow GenreEntity

instance Entity (GenreTable Result) where
  type NewEntity (GenreTable Result) = NewGenre
  type PrimaryKey (GenreTable Result) = GenreRef
  primaryKey e = e.id

data GenreParentTable f = GenreParentTable
  { genre_id :: Column f GenreRef,
    parent_genre :: Column f GenreRef
  }
  deriving (Generic, Rel8able)

type GenreParentEntity = GenreParentTable Result

data TopLevelGenreTable f = TopLevelGenreTable
  { id :: Column f GenreRef,
    name :: Column f Text,
    description :: Column f (Maybe Text),
    parents :: Column f ([(JSONBEncoded GenreLink)]),
    children :: Column f ([(JSONBEncoded GenreLink)])
  }
  deriving (Generic, Rel8able)

type TopLevelGenreEntity = TopLevelGenreTable Result
deriving via (FromGeneric TopLevelGenreEntity) instance TextShow TopLevelGenreEntity

data NewGenre = NewGenre
  { name :: Text,
    description :: Maybe Text,
    topLevel :: Bool
  }
  deriving (Generic, Eq, Show)
  deriving TextShow via FromGeneric NewGenre

instance From NewGenre (GenreTable Expr) where
  from new = GenreTable {
    id = function "uuid_generate_v4" (),
    name = lit new.name,
    description = lit new.description,
    top_level = lit new.topLevel
  }

data Genre = Genre
  { ref :: GenreRef
  , name :: Text
  , description :: Maybe Text
  , parents :: Vector GenreOrLink
  , children :: Vector GenreOrLink
  }
  deriving (Eq, Generic)
  deriving TextShow via FromGeneric Genre
  deriving ToJSON via CustomJSON JSONOptions Genre

data GenreLink = GenreLink
  { ref :: GenreRef
  , name :: Text
  }
  deriving (Eq, Generic, Show, Hashable)
  deriving TextShow via FromGeneric GenreLink
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions GenreLink

instance From GenreEntity GenreLink where
  from e = GenreLink
    { ref = e.id,
      name = e.name
    }

data GenreOrLink = ResolvedGenre Genre
  | UnresolvedGenre GenreLink
  deriving (Eq, Generic)
  deriving TextShow via FromGeneric GenreOrLink
  deriving ToJSON via CustomJSON '[SumUntaggedValue] GenreOrLink

mkGenre :: GenreEntity -> [GenreLink] -> [GenreLink] -> Genre
mkGenre e parents children = Genre {
  ref = e.id,
  name = e.name,
  description = e.description,
  parents = V.fromList (UnresolvedGenre <$> parents),
  children = V.fromList (UnresolvedGenre <$> children)
}

instance From TopLevelGenreEntity Genre where
  from e = Genre {
    ref = e.id,
    name = e.name,
    description = e.description,
    parents = V.fromList $ UnresolvedGenre . fromJSONBEncoded <$> e.parents,
    children = V.fromList $ UnresolvedGenre . fromJSONBEncoded <$> e.children
  }
