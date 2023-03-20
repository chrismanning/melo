{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Artist.Name.Types where

import Data.Hashable
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Text
import Data.UUID
import GHC.Generics
import Melo.Database.Repo
import Melo.Library.Artist.Types
import Rel8
import Witch

data ArtistNameTable f = ArtistNameTable
  { id :: Column f ArtistNameRef,
    artist_id :: Column f ArtistRef,
    name :: Column f Text
  }
  deriving (Generic, Rel8able)

type ArtistNameEntity = ArtistNameTable Result

deriving instance Show ArtistNameEntity

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

instance From NewArtistName (ArtistNameTable Expr) where
  from n =
    ArtistNameTable
      { id = nullaryFunction "uuid_generate_v4",
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

instance GQLType ArtistNameRef where
  type KIND ArtistNameRef = SCALAR

instance EncodeScalar ArtistNameRef where
  encodeScalar (ArtistNameRef uuid) = M.String $ toText uuid

instance DecodeScalar ArtistNameRef where
  decodeScalar (M.String s) = case fromText s of
    Nothing -> Left "ArtistNameRef must be UUID"
    Just uuid -> Right $ ArtistNameRef uuid
  decodeScalar _ = Left "ArtistNameRef must be a String"

instance From ArtistNameRef UUID where
  from (ArtistNameRef uuid) = uuid
