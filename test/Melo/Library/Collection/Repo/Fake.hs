{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Repo.Fake where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Text qualified as T
import Data.UUID.V4
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.Fake
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types
import Rel8 (Result)
import System.IO.Unsafe
import Witch

type FakeCollectionRepository = FakeRepository (CollectionTable Result)

newtype FakeCollectionRepositoryT m a = FakeCollectionRepositoryT {
  runFakeCollectionRepositoryT :: FakeRepositoryT (CollectionTable Result) m a
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
      MonadState FakeCollectionRepository,
      Repository (CollectionTable Result)
    )

instance Monad m => CollectionRepository (FakeCollectionRepositoryT m) where
  getByUri = undefined

instance From NewCollection CollectionEntity where
  from c@NewFilesystemCollection {name, watch} =
    CollectionTable
      { id = CollectionRef $ unsafePerformIO nextRandom,
        root_uri = T.pack $ show $ rootUri c,
        name = name,
        watch = watch,
        kind = kind c
      }
    where
      rootUri NewFilesystemCollection {rootPath} = fileUri $ T.unpack rootPath
      kind NewFilesystemCollection {} = "filesystem"

runFakeCollectionRepository :: Monad m => FakeRepository (CollectionTable Result) -> FakeCollectionRepositoryT m a -> m a
runFakeCollectionRepository fake = runFakeRepository fake . runFakeCollectionRepositoryT
