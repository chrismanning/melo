{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Repo.Fake where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Melo.Common.FileSystem.Watcher
import Melo.Database.Repo
import Melo.Database.Repo.Fake
import Melo.Metadata.Mapping.Types

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

--instance Monad m => TagMappingRepository (FakeTagMappingRepositoryT m) where

instance Monad m => FileSystemWatchLocks (FakeTagMappingRepositoryT m) where
  lockPathsDuring _ = id

runFakeTagMappingRepository :: Monad m => FakeRepository TagMappingEntity -> FakeTagMappingRepositoryT m a -> m a
runFakeTagMappingRepository fake = runFakeRepository fake . runFakeTagMappingRepositoryT
