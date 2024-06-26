{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Artist.Name.Types where

import Data.Hashable
import Data.UUID
import Melo.Database.Repo
import Melo.Library.Artist.Types
import Rel8

data ArtistNameTable f = ArtistNameTable
  { id :: Column f ArtistNameRef,
    artist_id :: Column f ArtistRef,
    name :: Column f Text
  }
  deriving (Generic, Rel8able)

type ArtistNameEntity = ArtistNameTable Result

deriving instance Show ArtistNameEntity

deriving via (FromGeneric ArtistNameEntity) instance TextShow ArtistNameEntity

deriving instance Ord ArtistNameEntity

deriving instance Eq ArtistNameEntity

deriving instance Hashable ArtistNameEntity

instance Entity ArtistNameEntity where
  type NewEntity ArtistNameEntity = NewArtistName
  type PrimaryKey ArtistNameEntity = ArtistNameRef
  primaryKey e = e.id

data NewArtistName = NewArtistName
  { artist :: ArtistRef,
    name :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via FromGeneric NewArtistName

instance From NewArtistName (ArtistNameTable Expr) where
  from n =
    ArtistNameTable
      { id = function "uuid_generate_v4" (),
        artist_id = lit n.artist,
        name = lit n.name
      }

fromNewArtistName :: NewArtistName -> UUID -> ArtistNameEntity
fromNewArtistName n ref =
  ArtistNameTable
    { id = ArtistNameRef ref,
      artist_id = n.artist,
      name = n.name
    }

newtype ArtistNameRef = ArtistNameRef UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)
  deriving (TextShow) via FromGeneric ArtistNameRef

instance From ArtistNameRef UUID where
  from (ArtistNameRef uuid) = uuid

instance From ArtistNameEntity ArtistLink where
  from e =
    ArtistLink
      { ref = e.artist_id,
        name = e.name
      }
