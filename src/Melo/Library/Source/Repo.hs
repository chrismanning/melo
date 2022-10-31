{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Source.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Functor.Contravariant
import Data.Pool
import Data.Text qualified as T
import Data.Vector (Vector, empty)
import Hasql.Connection
import Melo.Common.NaturalSort
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Types
import Melo.Library.Source.Types
import Rel8
  ( Name,
    TableSchema (..),
    Upsert (..),
    lit,
    (==.),
  )
import Rel8 qualified

class Repository SourceEntity m => SourceRepository m where
  getByUri :: Vector URI -> m (Vector SourceEntity)
  getKeysByUri :: Vector URI -> m (Vector (PrimaryKey SourceEntity))
  getByUriPrefix :: URI -> m (Vector SourceEntity)
  getKeysByUriPrefix :: URI -> m (Vector (PrimaryKey SourceEntity))
  getCollectionSources :: CollectionRef -> m (Vector SourceEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    SourceRepository m
  ) =>
  SourceRepository (t m)
  where
  getByUri = lift . getByUri
  getKeysByUri = lift . getKeysByUri
  getByUriPrefix = lift . getByUriPrefix
  getKeysByUriPrefix = lift . getKeysByUriPrefix
  getCollectionSources = lift . getCollectionSources

newtype SourceRepositoryIOT m a = SourceRepositoryIOT
  { runSourceRepositoryIOT :: RepositoryIOT SourceTable m a
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
      MonadReader (RepositoryHandle SourceTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance MonadIO m => Repository SourceEntity (SourceRepositoryIOT m) where
  getAll = SourceRepositoryIOT $ do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc $ orderByUri $ Rel8.each tbl
  getByKey ks = SourceRepositoryIOT $ do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      let keys = Rel8.lit <$> ks
      all <- orderByUri $ Rel8.each tbl
      Rel8.where_ $ pk all `Rel8.in_` keys
      pure all
  insert = pure . sortByUri <=< SourceRepositoryIOT . insert
  insert' = SourceRepositoryIOT . insert'
  delete = SourceRepositoryIOT . delete
  update = pure . sortByUri <=< SourceRepositoryIOT . update
  update' = SourceRepositoryIOT . update'

orderByUri :: Rel8.Query (SourceTable Rel8.Expr) -> Rel8.Query (SourceTable Rel8.Expr)
orderByUri = Rel8.orderBy (source_uri >$< Rel8.asc)

sortByUri :: Vector SourceEntity -> Vector SourceEntity
sortByUri = sortVectorNaturalBy source_uri

instance
  (MonadIO m) =>
  SourceRepository (SourceRepositoryIOT m)
  where
  getByUri us | null us = pure empty
  getByUri us = do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc do
      let us' = Rel8.lit . T.pack . show <$> us
      srcs <- orderByUri $ Rel8.each tbl
      Rel8.where_ $ srcs.source_uri `Rel8.in_` us'
      pure srcs
  getByUriPrefix prefix = do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc do
      srcs <- orderByUri $ Rel8.each tbl
      Rel8.where_ (srcs.source_uri `startsWith` Rel8.lit (T.pack $ show prefix))
      pure srcs
  getKeysByUri us | null us = pure empty
  getKeysByUri us = do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      let us' = Rel8.lit . T.pack . show <$> us
      srcs <- Rel8.each tbl
      Rel8.where_ $ srcs.source_uri `Rel8.in_` us'
      pure (pk srcs)
  getKeysByUriPrefix prefix = do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      srcs <- Rel8.each tbl
      Rel8.where_ (srcs.source_uri `startsWith` Rel8.lit (T.pack $ show prefix))
      pure (pk srcs)
  getCollectionSources collectionRef = do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc do
      srcs <- orderByUri $ Rel8.each tbl
      Rel8.where_ (srcs.collection_id ==. Rel8.lit collectionRef)
      pure srcs

sourceSchema :: TableSchema (SourceTable Name)
sourceSchema =
  TableSchema
    { name = "source",
      schema = Nothing,
      columns =
        SourceTable
          { id = "id",
            kind = "kind",
            metadata_format = "metadata_format",
            metadata = "metadata",
            source_uri = "source_uri",
            idx = "idx",
            time_range = "time_range",
            scanned = "scanned",
            collection_id = "collection_id",
            cover = "cover"
          }
    }

runSourceRepositoryPooledIO :: Pool Connection -> SourceRepositoryIOT m a -> m a
runSourceRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = sourceSchema,
        pk = (\e -> e.id),
        upsert =
          Just
            Upsert
              { index = \e -> (e.source_uri, e.idx),
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
    . runRepositoryIOT
    . runSourceRepositoryIOT

runSourceRepositoryIO :: Connection -> SourceRepositoryIOT m a -> m a
runSourceRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = sourceSchema,
        pk = (\e -> e.id),
        upsert =
          Just
            Upsert
              { index = \e -> (e.source_uri, e.idx),
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
    . runRepositoryIOT
    . runSourceRepositoryIOT
