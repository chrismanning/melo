{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Library.Release.Repo where

import Data.Pool
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasql.Session qualified as Hasql
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Release.Types
import Melo.Lookup.MusicBrainz qualified as MB
import OpenTelemetry.Trace qualified as Otel
import Rel8 (Expr, Query, lit, (==.), (||.))
import Rel8 qualified

class Repository ReleaseEntity m => ReleaseRepository m where
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe ReleaseEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ReleaseRepository m
  ) =>
  ReleaseRepository (t m)
  where
  getByMusicBrainzId = lift . getByMusicBrainzId

instance {-# OVERLAPPING #-} Repository ReleaseEntity (AppM IO IO) where
  getAll = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ReleaseTable
    withSpan ("getAll$" <> T.pack tbl.name) defaultSpanArguments do
      runSelect pool $ Rel8.each tbl
  getByKey ks | Prelude.length ks == 1 = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @ReleaseTable
    withSpan ("getByKey$" <> T.pack tbl.name) defaultSpanArguments do
      runSelect pool do
        all <- Rel8.each tbl
        let k = Rel8.lit $ V.head ks
        Rel8.where_ $ pk all ==. k
        pure all
  getByKey ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @ReleaseTable
    withSpan ("getByKey$" <> T.pack tbl.name) defaultSpanArguments do
      runSelect pool do
        let keys = Rel8.lit <$> ks
        all <- Rel8.each tbl
        Rel8.where_ $ pk all `Rel8.in_` keys
        pure all
  insert es | null es = pure mempty
  insert es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ReleaseTable
    withSpan ("insert$" <> T.pack tbl.name) defaultSpanArguments do
      V.fromList
        <$> runInsert
          pool
          Rel8.Insert
            { into = tbl,
              rows = Rel8.values (from <$> es),
              onConflict = Rel8.DoNothing,
              returning = Rel8.Projection (\x -> x)
            }
  insert' es | null es = pure 0
  insert' es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ReleaseTable
    withSpan ("insert'$" <> T.pack tbl.name) defaultSpanArguments do
      runInsert
        pool
        Rel8.Insert
          { into = tbl,
            rows = Rel8.values (from <$> es),
            onConflict = Rel8.DoNothing,
            returning = fromIntegral <$> Rel8.NumberOfRowsAffected
          }
  delete ks | null ks = pure mempty
  delete ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @ReleaseTable
    withSpan' ("delete$" <> T.pack tbl.name) defaultSpanArguments \span -> do
      let keys = Rel8.lit <$> ks
      let d =
            Rel8.Delete
              { from = tbl,
                using = pure (),
                deleteWhere = \_ row -> pk row `Rel8.in_` keys,
                returning = Rel8.Projection pk
              }
      do
        let statement = Rel8.showDelete d
        Otel.addAttributes span [("database.statement", Otel.toAttribute $ T.pack statement)]
        $(logDebugVIO ['statement]) "Executing DELETE"
      let session = Hasql.statement () $ Rel8.delete d
      liftIO do
        dels <- withResource pool $ \conn -> Hasql.run session conn >>= throwOnLeft
        pure $ V.fromList dels
  update es | null es = pure mempty
  update es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @ReleaseTable
    withSpan ("update$" <> T.pack tbl.name) defaultSpanArguments do
      us <- forM es $ \e ->
        doUpdate h pool (from e) (Rel8.Projection (\x -> x))
      pure $ V.fromList (concat us)
  update' es | null es = pure ()
  update' es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @ReleaseTable
    withSpan ("update'$" <> T.pack tbl.name) defaultSpanArguments do
      forM_ es $ \e -> doUpdate h pool (from e) (pure ())

instance ReleaseRepository (AppM IO IO) where
  getByMusicBrainzId mbid = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ReleaseTable
    firstOf traverse <$> runSelect pool do
      release <- Rel8.each tbl
      let mbid' = lit (Just mbid.mbid)
      Rel8.where_ $
        release.musicbrainz_id ==. mbid' ||. release.musicbrainz_group_id ==. mbid'
      pure release

releaseForRef :: Expr ReleaseRef -> Query (ReleaseTable Expr)
releaseForRef releaseRef = Rel8.filter (\release -> release.id ==. releaseRef) =<< Rel8.each releaseSchema

releaseSchema :: Rel8.TableSchema (ReleaseTable Rel8.Name)
releaseSchema =
  Rel8.TableSchema
    { name = "release",
      schema = Nothing,
      columns =
        ReleaseTable
          { id = "id",
            title = "title",
            comment = "comment",
            year_released = "year_released",
            original_year_released = "original_year_released",
            length = "length",
            musicbrainz_id = "musicbrainz_id",
            musicbrainz_group_id = "musicbrainz_group_id",
            kind = "kind",
            catalogue_number = "catalogue_number"
          }
    }

initReleaseRepo :: AppDataReader m => m ()
initReleaseRepo = putAppData
  RepositoryHandle
    { tbl = releaseSchema,
      pk = (.id),
      upsert =
        Just
          Rel8.Upsert
            { index = (.musicbrainz_id),
              set = \new old -> new & #id .~ old.id,
              updateWhere = \new old -> new.musicbrainz_id ==. old.musicbrainz_id
            }
    }
