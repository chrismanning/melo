{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Repo where

import Control.Exception.Safe (Exception)
import Control.Lens
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.Kind
import Data.Vector (Vector, singleton)
import Data.Vector.Lens ()

class Entity e where
  type NewEntity e :: Type
  type PrimaryKey e :: Type
  primaryKey :: e -> PrimaryKey e

class (Monad m, Entity e) => Repository e m | m -> e where
  getAll :: m (Vector e)
  getByKey :: Vector (PrimaryKey e) -> m (Vector e)
  insert :: Vector (NewEntity e) -> m (Vector e)
  insert' :: Vector (NewEntity e) -> m Int
  delete :: Vector (PrimaryKey e) -> m (Vector (PrimaryKey e))
  update :: Vector e -> m (Vector e)
  update' :: Vector e -> m ()

getSingle :: forall e m. Repository e m => PrimaryKey e -> m (Maybe e)
getSingle k = firstOf traverse <$> getByKey (singleton k)

insertSingle :: forall e m. Repository e m => NewEntity e -> m (Maybe e)
insertSingle e = firstOf traverse <$> insert (singleton e)

updateSingle :: forall e m. Repository e m => e -> m (Maybe e)
updateSingle e = firstOf traverse <$> update (singleton e)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    Repository ts m
  ) =>
  Repository ts (t m)
  where
  getAll = lift getAll
  getByKey = lift . getByKey
  insert = lift . insert
  insert' = lift . insert'
  delete = lift . delete
  update = lift . update
  update' = lift . update'

data DatabaseError = ConnectionError (Maybe ByteString)
  deriving (Show, Exception)
