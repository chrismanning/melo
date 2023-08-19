{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Repo.Fake where

import Control.Lens (iso, mapping)
import Control.Monad
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
import qualified Data.Vector as V
import Data.Vector.Lens
import Hasql.Connection qualified as Hasql
import Melo.Common.Exception
import Melo.Database.Repo
import Unsafe.Coerce
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
  getAll = V.fromList . H.elems . coerce <$> get
  getByKey ks = do
    sources <- coerce <$> get
    pure $ V.mapMaybe id $ fmap (`H.lookup` sources) ks
  insert es = let vs = fmap from es in state $ \(FakeRepository repo) ->
    (vs, FakeRepository $ foldl' (\r v -> H.insert (primaryKey v) v r) repo vs)
  insert' = fmap length . insert . unsafeCoerce
  delete ks = do
    modify $ \(FakeRepository repo) -> FakeRepository $ foldl' (flip H.delete) repo ks
    undefined
  update vs = state $ \(FakeRepository repo) ->
               (vs, FakeRepository $ foldl' (\r v -> H.insert (primaryKey v) v r) repo vs)
  update' = void . update

runFakeRepository :: Monad m => FakeRepository t -> FakeRepositoryT t m a -> m a
runFakeRepository fake = flip evalStateT fake . runFakeRepositoryT
