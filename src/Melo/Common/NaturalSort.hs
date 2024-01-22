module Melo.Common.NaturalSort where

import Algorithms.NaturalSort
import Algorithms.NaturalSort qualified as Natural (compare)
import Data.Vector (modify)
import Data.Vector.Algorithms.Intro (sortBy)

compareNaturalBy :: (NaturalSort b) => (a -> b) -> a -> a -> Ordering
compareNaturalBy f a b = Natural.compare (f a) (f b)

sortVectorNaturalBy :: (NaturalSort b) => (a -> b) -> Vector a -> Vector a
sortVectorNaturalBy f = modify (sortBy (compareNaturalBy f))
