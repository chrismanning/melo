{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.Mapping.Repo where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Foldl (PrimMonad)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
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

runTagMappingRepositoryIO :: DbConnection -> TagMappingRepositoryIOT m a -> m a
runTagMappingRepositoryIO connSrc =
  flip
    runReaderT
    RepositoryHandle
      { connSrc,
        tbl = tagMappingSchema,
        pk = (.name),
        upsert =
          Just
            Upsert
              { index = (.name),
                set = const,
                updateWhere = \new old -> new.name ==. old.name
              }
      }
    . runRepositoryIOT
    . runTagMappingRepositoryIOT
