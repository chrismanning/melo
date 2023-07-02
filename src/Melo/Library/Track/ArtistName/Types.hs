{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Track.ArtistName.Types where

import GHC.Generics
import Melo.Library.Artist.Name.Types (ArtistNameRef (..))
import Melo.Library.Track.Types (TrackRef (..))
import Rel8

data TrackArtistNameTable f = TrackArtistNameTable
  { track_id :: Column f TrackRef,
    artist_name_id :: Column f ArtistNameRef
  }
  deriving (Generic, Rel8able)

type TrackArtistNameEntity = TrackArtistNameTable Result

instance From TrackArtistNameEntity (TrackArtistNameTable Expr) where
  from e =
    TrackArtistNameTable
      { track_id = lit e.track_id,
        artist_name_id = lit e.artist_name_id
      }
