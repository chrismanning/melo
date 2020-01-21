module Melo.API.Haxl where

import Data.Pool
import Database.Beam.Postgres (Connection)
import GHC.Generics
import Haxl.Core

type Haxl = GenHaxl () ()

data Handle
  = Handle
      { connPool :: Pool Connection
      }
  deriving (Generic)
