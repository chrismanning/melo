{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Uuid where

import Data.UUID
import Data.UUID.V4
import Melo.Common.Monad

class (Monad m) => UuidGenerator m where
  generateV4 :: m UUID

instance
  {-# OVERLAPPABLE #-}
  ( MonadTrans t,
    Monad m,
    UuidGenerator m
  ) =>
  UuidGenerator (t m)
  where
  generateV4 = lift generateV4

instance UuidGenerator IO where
  generateV4 = nextRandom
