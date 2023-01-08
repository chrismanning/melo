{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Uuid where

import Control.Monad.Trans.Class
import Data.UUID
import Data.UUID.V4

class UuidGenerator m where
  generateV4 :: m UUID

--instance {-# OVERLAPPABLE #-} MonadIO m => UuidGenerator m where
--  generateV4 = liftIO nextRandom

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    Monad m,
    UuidGenerator m
  ) =>
  UuidGenerator (t m)
  where
  generateV4 = lift generateV4

instance UuidGenerator IO where
  generateV4 = nextRandom
