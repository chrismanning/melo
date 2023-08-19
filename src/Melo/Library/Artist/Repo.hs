{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Library.Artist.Repo where

import Control.Foldl (impurely, vectorM)
import Data.Coerce
import Data.Pool
import Data.Text qualified as T
import Data.Vector qualified as V
import Hasql.Session qualified as Hasql
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Types
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo (releaseForRef)
import Melo.Library.Release.Types
import Melo.Library.Source.Types (SourceRef)
import Melo.Library.Track.Repo (trackForSourceRef)
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz qualified as MB
import OpenTelemetry.Trace qualified as Otel
import Rel8
import Streaming.Prelude qualified as S

class Repository ArtistEntity m => ArtistRepository m where
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe ArtistEntity)
  getReleaseArtists :: ReleaseRef -> m (Vector (ArtistEntity, ArtistNameEntity))
  getSourceReleaseArtists :: SourceRef -> m (Vector (ArtistEntity, ArtistNameEntity))
  getByName :: Text -> m (Vector ArtistEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ArtistRepository m
  ) =>
  ArtistRepository (t m)
  where
  getByMusicBrainzId = lift . getByMusicBrainzId
  getReleaseArtists = lift . getReleaseArtists
  getSourceReleaseArtists = lift . getSourceReleaseArtists
  getByName = lift . getByName

instance {-# OVERLAPS #-} Repository ArtistEntity (AppM IO IO) where
  getAll = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ArtistTable
    withSpan ("getAll$" <> T.pack tbl.name) defaultSpanArguments do
      runSelect pool $ Rel8.each tbl
  getByKey ks | Prelude.length ks == 1 = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @ArtistTable
    withSpan ("getByKey$" <> T.pack tbl.name) defaultSpanArguments do
      runSelect pool do
        all <- Rel8.each tbl
        let k = Rel8.lit $ V.head ks
        Rel8.where_ $ pk all ==. k
        pure all
  getByKey ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @ArtistTable
    withSpan ("getByKey$" <> T.pack tbl.name) defaultSpanArguments do
      runSelect pool do
        let keys = Rel8.lit <$> ks
        all <- Rel8.each tbl
        Rel8.where_ $ pk all `Rel8.in_` keys
        pure all
  insert es | V.null es = pure V.empty
  insert es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ArtistTable
    S.each (insertArtists es tbl (Rel8.Projection (\x -> x)))
      & S.catMaybes
      & S.mapM (runInsert' pool)
      & S.mapMaybeM \case
        Left e -> do
          let cause = displayException e
          $(logWarnV ['cause]) "Artist insert failed"
          pure Nothing
        Right a -> pure $ Just a
      & S.concat
      & impurely S.foldM_ vectorM
  insert' es | V.null es = pure 0
  insert' es = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ArtistTable
    S.each (insertArtists es tbl (fromIntegral <$> Rel8.NumberOfRowsAffected))
      & S.catMaybes
      & S.mapM (runInsert' pool)
      & S.mapMaybeM \case
        Left e -> do
          let cause = displayException e
          $(logWarnV ['cause]) "Artist insert failed"
          pure Nothing
        Right a -> pure $ Just a
      & S.sum_
  delete ks | Prelude.null ks = pure mempty
  delete ks = do
    pool <- getConnectionPool
    RepositoryHandle {tbl, pk} <- getRepoHandle @ArtistTable
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
  update es | Prelude.null es = pure mempty
  update es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @ArtistTable
    withSpan ("update$" <> T.pack tbl.name) defaultSpanArguments do
      us <- forM es $ \e ->
        doUpdate h pool (Prelude.from e) (Rel8.Projection (\x -> x))
      pure $ V.fromList (concat us)
  update' es | Prelude.null es = pure ()
  update' es = do
    pool <- getConnectionPool
    h@RepositoryHandle {tbl} <- getRepoHandle @ArtistTable
    withSpan ("update'$" <> T.pack tbl.name) defaultSpanArguments do
      forM_ es $ \e -> doUpdate h pool (Prelude.from e) (pure ())

insertArtists :: forall a. Vector (NewEntity ArtistEntity) -> TableSchema (ArtistTable Name) -> Returning (ArtistTable Name) a -> [Maybe (Insert a)]
insertArtists as tbl returning =
  [ genIns True,
    genIns False
  ]
  where
    (a, b) = V.unstablePartition (isn't _Nothing . (.musicBrainzId)) as
    genIns :: Bool -> Maybe (Insert a)
    genIns True | V.null a = Nothing
    genIns False | V.null b = Nothing
    genIns incl =
      Just
        Insert
          { into = tbl,
            rows = Rel8.values (Prelude.from <$> (if incl then a else b)),
            onConflict = DoUpdate (partialUpsert incl),
            returning
          }
    partialUpsert False =
      PartialUpsert
        { index = (.name),
          indexWhere = \a -> isNull a.musicbrainz_id,
          set = \new old -> new & #id .~ old.id,
          updateWhere = \_new _old -> lit True
        }
    partialUpsert True =
      PartialUpsert
        { index = \a -> (a.name, a.musicbrainz_id),
          indexWhere = \a -> isNonNull a.musicbrainz_id,
          set = \new old -> new & #id .~ old.id,
          updateWhere = \_new _old -> lit True
        }

instance ArtistRepository (AppM IO IO) where
  getByMusicBrainzId mbid = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ArtistTable
    firstOf traverse <$> runSelect pool do
      artist <- Rel8.each tbl
      Rel8.where_ $ artist.musicbrainz_id ==. lit (Just $ coerce mbid)
      pure artist
  getReleaseArtists releaseRef = do
    pool <- getConnectionPool
    runSelect pool do
      artistName <- artistNameForReleaseRef (lit releaseRef)
      artist <- artistForRef artistName.artist_id
      pure (artist, artistName)
  getSourceReleaseArtists srcRef = do
    pool <- getConnectionPool
    runSelect pool do
      track <- trackForSourceRef (lit srcRef)
      release <- releaseForRef track.release_id
      artistName <- artistNameForReleaseRef release.id
      artist <- artistForRef artistName.artist_id
      pure (artist, artistName)
  getByName name = do
    pool <- getConnectionPool
    runSelect pool $
      artistForName (lit name)

artistForRef :: Expr ArtistRef -> Query (ArtistTable Expr)
artistForRef artistRef =
  Rel8.filter (\artist -> artist.id ==. artistRef) =<< each artistSchema

artistForName :: Expr Text -> Query (ArtistTable Expr)
artistForName artistName =
  Rel8.filter (\artist -> artist.name ==. artistName) =<< each artistSchema

artistSchema :: TableSchema (ArtistTable Name)
artistSchema =
  TableSchema
    { name = "artist",
      schema = Nothing,
      columns =
        ArtistTable
          { id = "id",
            name = "name",
            disambiguation = "disambiguation",
            short_bio = "short_bio",
            bio = "bio",
            country = "country",
            musicbrainz_id = "musicbrainz_id"
          }
    }

initArtistRepo :: AppDataReader m => m ()
initArtistRepo = putAppData
  RepositoryHandle
    { tbl = artistSchema,
      pk = (.id),
      upsert = Nothing
    }
