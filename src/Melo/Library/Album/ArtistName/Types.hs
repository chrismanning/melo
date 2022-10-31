{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Album.ArtistName.Types where

import GHC.Generics
import Melo.Library.Album.Types (AlbumRef(..))
import Melo.Library.Artist.Name.Types (ArtistNameRef(..))
import Rel8
import Witch

data AlbumArtistNameTable f = AlbumArtistNameTable
  { album_id :: Column f AlbumRef,
    artist_name_id :: Column f ArtistNameRef
  }
  deriving (Generic, Rel8able)

type AlbumArtistNameEntity = AlbumArtistNameTable Result

instance From AlbumArtistNameEntity (AlbumArtistNameTable Expr) where
  from e = AlbumArtistNameTable {
    album_id = lit e.album_id,
    artist_name_id = lit e.artist_name_id
  }
