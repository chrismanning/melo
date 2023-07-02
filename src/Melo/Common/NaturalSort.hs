module Melo.Common.NaturalSort where

import Algorithms.NaturalSort
import qualified Algorithms.NaturalSort as Natural (compare)
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector (modify)

compareNaturalBy :: NaturalSort b => (a -> b) -> a -> a -> Ordering
compareNaturalBy f a b = Natural.compare (f a) (f b)

sortVectorNaturalBy :: NaturalSort b => (a -> b) -> Vector a -> Vector a
sortVectorNaturalBy f = modify (sortBy (compareNaturalBy f))
