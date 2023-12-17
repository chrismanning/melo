{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Library.Collection.Repo where

import Control.Monad.State.Strict
import Data.Functor.Contravariant
import Data.Pool
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasql.Session qualified as Hasql
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.NaturalSort
import Melo.Common.Tracing
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Types
import Network.URI
import OpenTelemetry.Trace qualified as Otel
import Rel8
  ( Name,
    TableSchema (..),
    Upsert (..),
    in_,
    lit,
    (==.),
  )
import Rel8 qualified

class Repository CollectionEntity m => CollectionRepository m where
  getByUri :: Vector URI -> m (Vector CollectionEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    CollectionRepository m
  ) =>
  CollectionRepository (t m)
  where
  getByUri = lift . getByUri

instance {-# OVERLAPS #-} Repository CollectionEntity (AppM IO IO) where
  getAll = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @CollectionTable
    withSpan ("getAll$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool $ orderByUri $ Rel8.each tbl
  getByKey ks | length ks == 1 = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @CollectionTable
    withSpan ("getByKey$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        all <- Rel8.each tbl
        let k = Rel8.lit $ V.head ks
        Rel8.where_ $ pk all ==. k
        pure all
  getByKey ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @CollectionTable
    withSpan ("getByKey$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        let keys = Rel8.lit <$> ks
        all <- orderByUri $ Rel8.each tbl
        Rel8.where_ $ pk all `Rel8.in_` keys
        pure all
  insert es | null es = pure mempty
  insert es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, upsert} <- getRepoHandle @CollectionTable
    withSpan ("insert$" <> T.pack tbl.name.name) defaultSpanArguments do
      sortByUri
        <$> runInsert
          pool
          Rel8.runVector
          Rel8.Insert
            { into = tbl,
              rows = Rel8.values (from <$> es),
              onConflict = fromMaybe Rel8.Abort (Rel8.DoUpdate <$> upsert),
              returning = Rel8.Returning (\x -> x)
            }
  insert' es | null es = pure 0
  insert' es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, upsert} <- getRepoHandle @CollectionTable
    withSpan ("insert'$" <> T.pack tbl.name.name) defaultSpanArguments do
      runInsert
        pool
        Rel8.runN
        Rel8.Insert
          { into = tbl,
            rows = Rel8.values (from <$> es),
            onConflict = fromMaybe Rel8.Abort (Rel8.DoUpdate <$> upsert),
            returning = Rel8.NoReturning
          }
  delete ks | null ks = pure mempty
  delete ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @CollectionTable
    withSpan' ("delete$" <> T.pack tbl.name.name) defaultSpanArguments \span -> do
      let keys = Rel8.lit <$> ks
      let d =
            Rel8.Delete
              { from = tbl,
                using = pure (),
                deleteWhere = \_ row -> pk row `Rel8.in_` keys,
                returning = Rel8.Returning pk
              }
      do
        let statement = Rel8.showDelete d
        Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
        $(logDebugVIO ['statement]) "Executing DELETE"
      let session = Hasql.statement () $ Rel8.runVector $ Rel8.delete d
      liftIO do
        withResource pool $ \conn -> Hasql.run session conn >>= throwOnLeft
  update es | null es = pure mempty
  update es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @CollectionTable
    withSpan ("update$" <> T.pack tbl.name.name) defaultSpanArguments do
      us <- forM es $ \e ->
        doUpdate h pool Rel8.runVector (from e) (Rel8.Returning (\x -> x))
      pure (msum us)
  update' es | null es = pure ()
  update' es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @CollectionTable
    withSpan ("update'$" <> T.pack tbl.name.name) defaultSpanArguments do
      forM_ es $ \e -> doUpdate h pool Rel8.run_ (from e) Rel8.NoReturning

orderByUri :: Rel8.Query (CollectionTable Rel8.Expr) -> Rel8.Query (CollectionTable Rel8.Expr)
orderByUri = Rel8.orderBy (root_uri >$< Rel8.asc)

sortByUri :: Vector CollectionEntity -> Vector CollectionEntity
sortByUri = sortVectorNaturalBy (\e -> e.root_uri)

instance CollectionRepository (AppM IO IO) where
  getByUri us | null us = pure mempty
  getByUri fs = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @CollectionTable
    withSpan ("getByUri$" <> T.pack tbl.name.name) defaultSpanArguments do
      let q = Rel8.filter (\c -> c.root_uri `in_` fmap (lit . showt) fs) =<< Rel8.each tbl
      runSelect pool $ orderByUri q

collectionSchema :: TableSchema (CollectionTable Name)
collectionSchema =
  TableSchema
    { name = "collection",
      columns =
        CollectionTable
          { id = "id",
            root_uri = "root_uri",
            name = "name",
            watch = "watch",
            kind = "kind",
            rescan = "rescan",
            library = "library"
          }
    }

initCollectionRepo :: AppDataReader m => m ()
initCollectionRepo =
  putAppData
    RepositoryHandle
      { tbl = collectionSchema,
        pk = \e -> e.id,
        upsert =
          Just
            Upsert
              { index = \c -> c.root_uri,
                predicate = Nothing,
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
