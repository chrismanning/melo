{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Metadata.Mapping.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
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
      MonadReader (RepositoryHandle TagMappingTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad,
      Repository TagMappingEntity
    )

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
