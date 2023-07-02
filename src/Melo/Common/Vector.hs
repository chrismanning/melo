module Melo.Common.Vector where

import Data.Either.Combinators

lefts :: Vector (Either a b) -> Vector a
lefts = mapMaybe leftToMaybe

rights :: Vector (Either a b) -> Vector b
rights = mapMaybe rightToMaybe
