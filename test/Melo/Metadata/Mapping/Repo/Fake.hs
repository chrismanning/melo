{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Repo.Fake where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Melo.Database.Repo
import Melo.Database.Repo.Fake
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Types
import Rel8 (Result)

type FakeTagMappingRepository = FakeRepository TagMappingEntity

newtype FakeTagMappingRepositoryT m a = FakeTagMappingRepositoryT {
  runFakeTagMappingRepositoryT :: FakeRepositoryT TagMappingEntity m a
} deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadState FakeTagMappingRepository,
      Repository TagMappingEntity
    )

instance Monad m => TagMappingRepository (FakeTagMappingRepositoryT m) where

runFakeTagMappingRepository :: Monad m => FakeRepository TagMappingEntity -> FakeTagMappingRepositoryT m a -> m a
runFakeTagMappingRepository fake = runFakeRepository fake . runFakeTagMappingRepositoryT
