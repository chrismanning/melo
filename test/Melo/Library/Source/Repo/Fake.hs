{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Repo.Fake where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Melo.Database.Repo
import Melo.Database.Repo.Fake
import Melo.Library.Source.Repo
import Melo.Library.Source.Types
import Rel8 (Result)

type FakeSourceRepository = FakeRepository (SourceTable Result)

newtype FakeSourceRepositoryT m a = FakeSourceRepositoryT {
  runFakeSourceRepositoryT :: FakeRepositoryT (SourceTable Result) m a
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
      MonadState FakeSourceRepository,
      Repository (SourceTable Result)
    )

instance Monad m => SourceRepository (FakeSourceRepositoryT m) where
  getByUri = undefined
  getKeysByUri = undefined
  getByUriPrefix = undefined
  getKeysByUriPrefix = undefined
  getCollectionSources = undefined

runFakeSourceRepository :: Monad m => FakeRepository (SourceTable Result) -> FakeSourceRepositoryT m a -> m a
runFakeSourceRepository fake = runFakeRepository fake . runFakeSourceRepositoryT
