{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Collection.Repo where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Foldl (PrimMonad)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Functor.Contravariant
import Data.Text qualified as T
import Data.Vector (Vector, empty)
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
import Rel8 qualified

class Repository (CollectionTable Result) m => CollectionRepository m where
  getByUri :: Vector URI -> m (Vector (CollectionTable Result))

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
      MonadReader (RepositoryHandle CollectionTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance MonadIO m => Repository CollectionEntity (CollectionRepositoryIOT m) where
  getAll = CollectionRepositoryIOT $ do
    RepositoryHandle {connSrc, tbl} <- ask
    runSelect connSrc $ orderByUri $ Rel8.each tbl
  getByKey ks = CollectionRepositoryIOT $ do
    RepositoryHandle {connSrc, tbl, pk} <- ask
    runSelect connSrc do
      let keys = Rel8.lit <$> ks
      all <- orderByUri $ Rel8.each tbl
      Rel8.where_ $ pk all `Rel8.in_` keys
      pure all
  insert = pure . sortByUri <=< CollectionRepositoryIOT . insert
  insert' = CollectionRepositoryIOT . insert' @CollectionEntity
  delete = CollectionRepositoryIOT . delete @CollectionEntity
  update = pure . sortByUri <=< CollectionRepositoryIOT . update
  update' = CollectionRepositoryIOT . update'

orderByUri :: Rel8.Query (CollectionTable Rel8.Expr) -> Rel8.Query (CollectionTable Rel8.Expr)
orderByUri = Rel8.orderBy (root_uri >$< Rel8.asc)

sortByUri :: Vector CollectionEntity -> Vector CollectionEntity
sortByUri = sortVectorNaturalBy (\e -> e.root_uri)

instance MonadIO m => CollectionRepository (CollectionRepositoryIOT m) where
  getByUri us | null us = pure empty
  getByUri fs = do
    RepositoryHandle {connSrc, tbl} <- ask
    let q = Rel8.filter (\c -> c.root_uri `in_` fmap (lit . T.pack . show) fs) =<< Rel8.each tbl
    runSelect connSrc $ orderByUri q

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

runCollectionRepositoryIO :: DbConnection -> CollectionRepositoryIOT m a -> m a
runCollectionRepositoryIO connSrc =
  flip
    runReaderT
    RepositoryHandle
      { connSrc,
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
