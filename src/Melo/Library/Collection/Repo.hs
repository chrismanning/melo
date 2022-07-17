{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool
import qualified Data.Text as T
import Hasql.Connection
import Melo.Common.NaturalSort
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Types
import Network.URI
import Rel8
  ( Name,
    Result,
    TableSchema (..),
    Upsert (..),
    in_,
    lit,
    (==.),
  )
import qualified Rel8

class Repository (CollectionTable Result) m => CollectionRepository m where
  getByUri :: [URI] -> m [CollectionTable Result]

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    CollectionRepository m
  ) =>
  CollectionRepository (t m)
  where
  getByUri = lift . getByUri

newtype CollectionRepositoryIOT m a = CollectionRepositoryIOT
  { runCollectionRepositoryIOT :: RepositoryIOT CollectionTable m a
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
      MonadReader (RepositoryHandle CollectionTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl
    )

instance MonadIO m => Repository (CollectionTable Result) (CollectionRepositoryIOT m) where
  getAll = sortByUri <$> CollectionRepositoryIOT getAll
  getByKey = pure . sortByUri <=< CollectionRepositoryIOT . getByKey
  insert = pure . sortByUri <=< CollectionRepositoryIOT . insert
  insert' = CollectionRepositoryIOT . insert'
  delete = CollectionRepositoryIOT . delete
  update = pure . sortByUri <=< CollectionRepositoryIOT . update
  update' = CollectionRepositoryIOT . update'

sortByUri :: [CollectionTable Result] -> [CollectionTable Result]
sortByUri = sortNaturalBy (\e -> e.root_uri)

instance MonadIO m => CollectionRepository (CollectionRepositoryIOT m) where
  getByUri [] = pure []
  getByUri fs = do
    RepositoryHandle {connSrc, tbl} <- ask
    let q = Rel8.filter (\c -> c.root_uri `in_` fmap (lit . T.pack . show) fs) =<< Rel8.each tbl
    sortByUri <$> runSelect connSrc q

collectionSchema :: TableSchema (CollectionTable Name)
collectionSchema =
  TableSchema
    { name = "collection",
      schema = Nothing,
      columns =
        CollectionTable
          { id = "id",
            root_uri = "root_uri",
            name = "name",
            watch = "watch",
            kind = "kind"
          }
    }

runCollectionRepositoryPooledIO :: Pool Connection -> CollectionRepositoryIOT m a -> m a
runCollectionRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = collectionSchema,
        pk = \e -> e.id,
        upsert =
          Just
            Upsert
              { index = \c -> c.root_uri,
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
    . runRepositoryIOT
    . runCollectionRepositoryIOT

runCollectionRepositoryIO :: Connection -> CollectionRepositoryIOT m a -> m a
runCollectionRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = collectionSchema,
        pk = \e -> e.id,
        upsert =
          Just
            Upsert
              { index = \c -> c.root_uri,
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
    . runRepositoryIOT
    . runCollectionRepositoryIOT
