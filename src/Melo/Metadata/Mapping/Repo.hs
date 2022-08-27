{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Pool
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Metadata.Mapping.Types
import Rel8
  ( Name,
    TableSchema (..),
    Upsert (..),
    (==.),
  )

class Repository TagMappingEntity m => TagMappingRepository m

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TagMappingRepository m
  ) =>
  TagMappingRepository (t m)

newtype TagMappingRepositoryIOT m a = TagMappingRepositoryIOT
  { runTagMappingRepositoryIOT :: RepositoryIOT TagMappingTable m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadParallel,
      MonadReader (RepositoryHandle TagMappingTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadUnliftIO
    )

instance MonadIO m => Repository TagMappingEntity (TagMappingRepositoryIOT m) where
  getAll = TagMappingRepositoryIOT getAll
  getByKey = TagMappingRepositoryIOT . getByKey
  insert = TagMappingRepositoryIOT . insert
  insert' = TagMappingRepositoryIOT . insert'
  delete = TagMappingRepositoryIOT . delete
  update = TagMappingRepositoryIOT . update
  update' = TagMappingRepositoryIOT . update'

instance MonadIO m => TagMappingRepository (TagMappingRepositoryIOT m)

tagMappingSchema :: TableSchema (TagMappingTable Name)
tagMappingSchema =
  TableSchema
    { name = "tag_mapping",
      schema = Nothing,
      columns =
        TagMappingTable
          { name = "name",
            field_mappings = "field_mappings"
          }
    }

runTagMappingRepositoryPooledIO :: Pool Connection -> TagMappingRepositoryIOT m a -> m a
runTagMappingRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = tagMappingSchema,
        pk = \e -> e.name,
        upsert =
          Just
            Upsert
              { index = \e -> e.name,
                set = const,
                updateWhere = \new old -> new.name ==. old.name
              }
      }
    . runRepositoryIOT
    . runTagMappingRepositoryIOT

runTagMappingRepositoryIO :: Connection -> TagMappingRepositoryIOT m a -> m a
runTagMappingRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = tagMappingSchema,
        pk = \e -> e.name,
        upsert =
          Just
            Upsert
              { index = \e -> e.name,
                set = const,
                updateWhere = \new old -> new.name ==. old.name
              }
      }
    . runRepositoryIOT
    . runTagMappingRepositoryIOT
