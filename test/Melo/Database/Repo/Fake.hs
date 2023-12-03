{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Repo.Fake where

import Control.Monad
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Database.Repo
import Witch

newtype FakeRepository e = FakeRepository (HashMap (PrimaryKey e) e)

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
  getAll = V.fromList . HashMap.elems . coerce <$> get
  getByKey ks = do
    sources <- coerce <$> get
    pure $ V.mapMaybe id $ fmap (`HashMap.lookup` sources) ks
  insert es = let vs = fmap from es in state $ \(FakeRepository repo) ->
    (vs, FakeRepository $ foldl' (\r v -> HashMap.insert (primaryKey v) v r) repo vs)
  insert' es = let vs = fmap from es in state $ \(FakeRepository repo) ->
    (V.length vs, FakeRepository $ foldl' (\r v -> HashMap.insert (primaryKey v) v r) repo vs)
  delete ks = do
    modify $ \(FakeRepository repo) -> FakeRepository $ foldl' (flip HashMap.delete) repo ks
    undefined
  update vs = state $ \(FakeRepository repo) ->
                (vs, FakeRepository $ foldl' (\r v -> HashMap.insert (primaryKey v) v r) repo vs)
  update' = void . update

runFakeRepository :: Monad m => FakeRepository t -> FakeRepositoryT t m a -> m a
runFakeRepository fake = flip evalStateT fake . runFakeRepositoryT
