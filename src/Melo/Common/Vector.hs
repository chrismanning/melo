module Melo.Common.Vector where

import Data.Either.Combinators
import Data.Vector

lefts :: Vector (Either a b) -> Vector a
lefts = mapMaybe leftToMaybe

rights :: Vector (Either a b) -> Vector b
rights = mapMaybe rightToMaybe
