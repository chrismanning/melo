module Melo.Common.NaturalSort where

import Algorithms.NaturalSort
import qualified Algorithms.NaturalSort as Natural (compare)
import Data.List (sortBy)
import Data.Vector

compareNaturalBy :: NaturalSort b => (a -> b) -> a -> a -> Ordering
compareNaturalBy f a b = Natural.compare (f a) (f b)

sortNaturalBy :: NaturalSort b => (a -> b) -> [a] -> [a]
sortNaturalBy = sortBy . compareNaturalBy

sortVectorNaturalBy :: NaturalSort b => (a -> b) -> Vector a -> Vector a
sortVectorNaturalBy f c = fromList $ sortBy (compareNaturalBy f) (toList c)
