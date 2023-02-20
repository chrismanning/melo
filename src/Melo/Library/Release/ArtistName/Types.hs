{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Release.ArtistName.Types where

import GHC.Generics
import Melo.Library.Artist.Name.Types (ArtistNameRef (..))
import Melo.Library.Release.Types (ReleaseRef (..))
import Rel8
import Witch

data ReleaseArtistNameTable f = ReleaseArtistNameTable
  { release_id :: Column f ReleaseRef,
    artist_name_id :: Column f ArtistNameRef
  }
  deriving (Generic, Rel8able)

type ReleaseArtistNameEntity = ReleaseArtistNameTable Result

pattern ReleaseArtistNameEntity :: ReleaseRef -> ArtistNameRef -> ReleaseArtistNameEntity
pattern ReleaseArtistNameEntity {release_id, artist_name_id} = ReleaseArtistNameTable {..}

instance From ReleaseArtistNameEntity (ReleaseArtistNameTable Expr) where
  from e =
    ReleaseArtistNameTable
      { release_id = lit e.release_id,
        artist_name_id = lit e.artist_name_id
      }
