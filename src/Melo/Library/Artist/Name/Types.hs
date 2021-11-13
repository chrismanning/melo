{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Artist.Name.Types where

import Data.Text
import Data.UUID
import GHC.Generics
import Rel8

data ArtistNameTable f = ArtistNameTable
  { id :: Column f UUID,
    artist_id :: Column f UUID,
    name :: Column f Text
  }
  deriving (Generic, Rel8able)

data AlbumArtistNameT f = AlbumArtistName
  { album_id :: Column f UUID,
    artist_name_id :: Column f UUID
  }
  deriving (Generic, Rel8able)
