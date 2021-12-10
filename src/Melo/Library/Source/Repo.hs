{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad.Base
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool
import qualified Data.Text as T
import Hasql.Connection
import Melo.Common.Logging
import Melo.Common.NaturalSort
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Types
import Melo.Library.Source.Types
import Rel8
  ( (==.),
    Name,
    TableSchema(..),
    Upsert(..),
    lit,
  )
import qualified Rel8

class Repository SourceEntity m => SourceRepository m where
  getByUri :: [URI] -> m [SourceEntity]
  getKeysByUri :: [URI] -> m [PrimaryKey SourceEntity]
  getByUriPrefix :: URI -> m [SourceEntity]
  getKeysByUriPrefix :: URI -> m [PrimaryKey SourceEntity]
  getCollectionSources :: CollectionRef -> m [SourceEntity]

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
      MonadParallel,
      MonadReader (RepositoryHandle SourceTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl
    )

instance MonadIO m => Repository SourceEntity (SourceRepositoryIOT m) where
  getAll = sortByUri <$> SourceRepositoryIOT getAll
  getByKey = pure . sortByUri <=< SourceRepositoryIOT . getByKey
  insert = pure . sortByUri <=< SourceRepositoryIOT . insert
  insert' = SourceRepositoryIOT . insert'
  delete = SourceRepositoryIOT . delete
  update = pure . sortByUri <=< SourceRepositoryIOT . update
  update' = SourceRepositoryIOT . update'

sortByUri :: [SourceEntity] -> [SourceEntity]
sortByUri = sortNaturalBy (^. #source_uri)

instance
  (MonadIO m) =>
  SourceRepository (SourceRepositoryIOT m)
  where
  getByUri [] = pure []
  getByUri us = sortByUri <$> do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc do
      let us' = Rel8.lit . T.pack . show <$> us
      srcs <- Rel8.each tbl
      Rel8.where_ $ (srcs ^. #source_uri) `Rel8.in_` us'
      pure srcs
  getByUriPrefix prefix = sortByUri <$> do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc do
      srcs <- Rel8.each tbl
      Rel8.where_ (srcs ^. #source_uri `startsWith` Rel8.lit (T.pack $ show prefix))
      pure srcs
  getKeysByUri [] = pure []
  getKeysByUri us = do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      let us' = Rel8.lit . T.pack . show <$> us
      srcs <- Rel8.each tbl
      Rel8.where_ $ (srcs ^. #source_uri) `Rel8.in_` us'
      pure (pk srcs)
  getKeysByUriPrefix prefix = do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      srcs <- Rel8.each tbl
      Rel8.where_ (srcs ^. #source_uri `startsWith` Rel8.lit (T.pack $ show prefix))
      pure (pk srcs)
  getCollectionSources (CollectionRef collectionId) = do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc do
      srcs <- Rel8.each tbl
      Rel8.where_ (srcs ^. #collection_id ==. Rel8.lit collectionId)
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
            collection_id = "collection_id"
          }
    }

runSourceRepositoryPooledIO :: Pool Connection -> SourceRepositoryIOT m a -> m a
runSourceRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = sourceSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = \e -> (e ^. #source_uri, e ^. #idx),
                set = const,
                updateWhere = \new old -> new ^. #id ==. old ^. #id
              }
      }
    . runRepositoryIOT . runSourceRepositoryIOT

runSourceRepositoryIO :: Connection -> SourceRepositoryIOT m a -> m a
runSourceRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = sourceSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = \e -> (e ^. #source_uri, e ^. #idx),
                set = const,
                updateWhere = \new old -> new ^. #id ==. old ^. #id
              }
      }
    . runRepositoryIOT . runSourceRepositoryIOT
