{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Melo.Database.Repo where

import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Int
import Data.Typeable
import Data.Vector.Lens ()
import Melo.Common.Exception (Exception)

class Entity e where
  type NewEntity e :: Type
  type PrimaryKey e :: Type
  primaryKey :: e -> PrimaryKey e

class (Monad m, Entity e) => Repository e m where
  getAll :: m (Vector e)
  getByKey :: Vector (PrimaryKey e) -> m (Vector e)
  insert :: Vector (NewEntity e) -> m (Vector e)
  insert' :: Vector (NewEntity e) -> m Int64
  delete :: Vector (PrimaryKey e) -> m (Vector (PrimaryKey e))
  update :: Vector e -> m (Vector e)
  update' :: Vector e -> m ()

getSingle :: forall e m. Repository e m => PrimaryKey e -> m (Maybe e)
getSingle k = firstOf traverse <$> getByKey (pure k)

insertSingle :: forall e m. Repository e m => NewEntity e -> m (Maybe e)
insertSingle e = firstOf traverse <$> insert (pure e)

insertSingle' :: forall e m. Repository e m => NewEntity e -> m ()
insertSingle' e = void $ insert @e (pure e)

updateSingle :: forall e m. Repository e m => e -> m (Maybe e)
updateSingle e = firstOf traverse <$> update (pure e)

updateSingle' :: forall e m. Repository e m => e -> m ()
updateSingle' e = void $ update @e (pure e)

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
  insert' = lift . insert' @ts
  delete = lift . delete @ts
  update = lift . update
  update' = lift . update'

data DatabaseError = ConnectionError (Maybe ByteString)
  | DatabaseNotConfigured
  | EntityNotConfigured TypeRep
  | OtherDatabaseError
  deriving (Show, Exception)
  deriving TextShow via FromStringShow DatabaseError
