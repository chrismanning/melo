{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Repo.Fake where

import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Foldable
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Melo.Database.Repo
import Witch

newtype FakeRepository e = FakeRepository (H.HashMap (PrimaryKey e) e)

newtype FakeRepositoryT e m a = FakeRepositoryT {
  runFakeRepositoryT :: StateT (FakeRepository e) m a
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
      MonadState (FakeRepository e)
    )

instance (
  Monad m,
  Entity e,
  Eq (PrimaryKey e),
  Hashable (PrimaryKey e),
  From (NewEntity e) e
  ) => Repository e (FakeRepositoryT e m) where
  getAll = H.elems . coerce <$> get
  getByKey ks = do
    sources <- coerce <$> get
    pure $ catMaybes $ fmap (`H.lookup` sources) ks
  insert es = let vs = fmap from es in state $ \(FakeRepository repo) ->
    (vs, FakeRepository $ foldl' (\r v -> H.insert (primaryKey v) v r) repo vs)
  insert' = void . insert
  delete ks = modify $ \(FakeRepository repo) -> FakeRepository $ foldl' (flip H.delete) repo ks
  update vs = state $ \(FakeRepository repo) ->
               (vs, FakeRepository $ foldl' (\r v -> H.insert (primaryKey v) v r) repo vs)
  update' = void . update

runFakeRepository :: Monad m => FakeRepository t -> FakeRepositoryT t m a -> m a
runFakeRepository fake = flip evalStateT fake . runFakeRepositoryT
