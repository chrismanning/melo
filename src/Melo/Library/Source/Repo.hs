{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Library.Source.Repo where

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
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Collection.Types
import Melo.Library.Source.Types
import OpenTelemetry.Trace qualified as Otel
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

instance {-# OVERLAPS #-} Repository SourceEntity (AppM IO IO) where
  getAll = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getAll$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool $ orderByUri $ Rel8.each tbl
  getByKey ks | V.length ks == 0 = pure mempty
  getByKey ks | V.length ks == 1 = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getByKey$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        srcs <- Rel8.each tbl
        let k = Rel8.lit $ V.head ks
        Rel8.where_ $ srcs.id ==. k
        pure srcs
  getByKey ks = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getByKey$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool (sourcesByKeys ks)
  insert es | null es = pure mempty
  insert es = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    RepositoryHandle {upsert} <- getRepoHandle @SourceTable
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
    let !tbl = sourceSchema
    RepositoryHandle {upsert} <- getRepoHandle @SourceTable
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
    let !tbl = sourceSchema
    withSpan' ("delete$" <> T.pack tbl.name.name) defaultSpanArguments \span -> do
      let keys = Rel8.lit <$> ks
      let d =
            Rel8.Delete
              { from = tbl,
                using = pure (),
                deleteWhere = \_ row -> row.id `Rel8.in_` keys,
                returning = Rel8.Returning (.id)
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
    let !tbl = sourceSchema
    h <- getRepoHandle @SourceTable
    withSpan ("update$" <> T.pack tbl.name.name) defaultSpanArguments do
      us <- forM es $ \e ->
        doUpdate h pool Rel8.runVector (from e) (Rel8.Returning (\x -> x))
      pure (msum us)
  update' es | null es = pure ()
  update' es = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    h <- getRepoHandle @SourceTable
    withSpan ("update'$" <> T.pack tbl.name.name) defaultSpanArguments do
      forM_ es $ \e -> doUpdate h pool Rel8.run_ (from e) Rel8.NoReturning

sourcesByKeys :: (Foldable f, Monad f) => f SourceRef -> Rel8.Query (SourceTable Rel8.Expr)
sourcesByKeys ks = do
  let !keys = Rel8.lit <$!> ks
  srcs <- orderByUri $ Rel8.each sourceSchema
  Rel8.where_ $ srcs.id `Rel8.in_` keys
  pure srcs

orderByUri :: Rel8.Query (SourceTable Rel8.Expr) -> Rel8.Query (SourceTable Rel8.Expr)
orderByUri = Rel8.orderBy (source_uri >$< Rel8.asc)

sortByUri :: Vector SourceEntity -> Vector SourceEntity
sortByUri = sortVectorNaturalBy source_uri

instance SourceRepository (AppM IO IO) where
  getByUri us | null us = pure mempty
  getByUri us = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getByUri$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        let us' = Rel8.lit . showt <$> us
        srcs <- orderByUri $ Rel8.each tbl
        Rel8.where_ $ srcs.source_uri `Rel8.in_` us'
        pure srcs
  getByUriPrefix prefix = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getByUriPrefix$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        srcs <- orderByUri $ Rel8.each tbl
        Rel8.where_ (srcs.source_uri `startsWith` Rel8.lit (showt prefix))
        pure srcs
  getKeysByUri us | null us = pure mempty
  getKeysByUri us = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getKeysByUri$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        let us' = Rel8.lit . showt <$> us
        srcs <- Rel8.each tbl
        Rel8.where_ $ srcs.source_uri `Rel8.in_` us'
        pure srcs.id
  getKeysByUriPrefix prefix = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getKeysByUriPrefix$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        srcs <- Rel8.each tbl
        Rel8.where_ (srcs.source_uri `startsWith` Rel8.lit (showt prefix))
        pure srcs.id
  getCollectionSources collectionRef = do
    pool <- getConnectionPool
    let !tbl = sourceSchema
    withSpan ("getCollectionSources$" <> T.pack tbl.name.name) defaultSpanArguments do
      runSelect pool do
        srcs <- orderByUri $ Rel8.each tbl
        Rel8.where_ (srcs.collection_id ==. Rel8.lit collectionRef)
        pure srcs

sourceSchema :: TableSchema (SourceTable Name)
sourceSchema =
  TableSchema
    { name = "source",
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

initSourceRepo :: AppDataReader m => m ()
initSourceRepo =
  putAppData
    RepositoryHandle
      { tbl = sourceSchema,
        pk = (\e -> e.id),
        upsert =
          Just
            Upsert
              { index = \e -> (e.source_uri, e.idx),
                predicate = Nothing,
                set = const,
                updateWhere = \new old -> new.id ==. old.id
              }
      }
