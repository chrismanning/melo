{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Artist.Related.Types where

import Data.UUID
import GHC.Generics
import Rel8

data RelatedArtistTable f = RelatedArtistTable
  { artist_id :: Column f UUID,
    related_artist_id :: Column f UUID
  }
  deriving (Generic, Rel8able)
