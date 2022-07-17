{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Repo where

import Control.Exception.Safe (Exception)
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.Kind
import Data.Maybe (listToMaybe)

class Entity e where
  type NewEntity e :: Type
  type PrimaryKey e :: Type
  primaryKey :: e -> PrimaryKey e

class (Monad m, Entity e) => Repository e m | m -> e where
  getAll :: m [e]
  getByKey :: [PrimaryKey e] -> m [e]
  insert :: [NewEntity e] -> m [e]
  insert' :: [NewEntity e] -> m ()
  delete :: [PrimaryKey e] -> m ()
  update :: [e] -> m [e]
  update' :: [e] -> m ()

getSingle :: Repository e m => PrimaryKey e -> m (Maybe e)
getSingle k = listToMaybe <$> getByKey [k]

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
