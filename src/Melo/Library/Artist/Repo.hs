{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

import Control.Foldl (impurely, vectorM)
import Data.Coerce
import Data.Vector qualified as V
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo (releaseForRef)
import Melo.Library.Release.Types
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Types
import Melo.Library.Source.Types (SourceRef)
import Melo.Library.Track.Repo (trackForSourceRef)
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz qualified as MB
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

newtype ArtistRepositoryIOT m a = ArtistRepositoryIOT
  { runArtistRepositoryIOT :: RepositoryIOT ArtistTable m a
  }
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader (RepositoryHandle ArtistTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance (MonadIO m, PrimMonad m, Logging m) => Repository ArtistEntity (ArtistRepositoryIOT m) where
  getAll = ArtistRepositoryIOT Repo.getAll
  getByKey = ArtistRepositoryIOT . Repo.getByKey
  insert es | V.null es = pure V.empty
  insert es = do
    RepositoryHandle {connSrc, tbl} <- ask
    S.each (insertArtists es tbl (Rel8.Projection (\x -> x)))
      & S.catMaybes
      & S.mapM (runInsert' connSrc)
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
    RepositoryHandle {connSrc, tbl} <- ask
    S.each (insertArtists es tbl (fromIntegral <$> Rel8.NumberOfRowsAffected))
      & S.catMaybes
      & S.mapM (runInsert' connSrc)
      & S.mapMaybeM \case
        Left e -> do
          let cause = displayException e
          $(logWarnV ['cause]) $ "Artist insert failed"
          pure Nothing
        Right a -> pure $ Just a
      & S.sum_
  delete = ArtistRepositoryIOT . Repo.delete @ArtistEntity
  update = ArtistRepositoryIOT . Repo.update
  update' = ArtistRepositoryIOT . Repo.update'

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

instance
  ( MonadIO m,
    PrimMonad m,
    Logging m
  ) =>
  ArtistRepository (ArtistRepositoryIOT m)
  where
  getByMusicBrainzId mbid = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
      artist <- Rel8.each tbl
      Rel8.where_ $ artist.musicbrainz_id ==. lit (Just $ coerce mbid)
      pure artist
  getReleaseArtists releaseRef = do
    RepositoryHandle {connSrc} <- ask
    runSelect connSrc do
      artistName <- artistNameForReleaseRef (lit releaseRef)
      artist <- artistForRef artistName.artist_id
      pure (artist, artistName)
  getSourceReleaseArtists srcRef = do
    RepositoryHandle {connSrc} <- ask
    runSelect connSrc do
      track <- trackForSourceRef (lit srcRef)
      release <- releaseForRef track.release_id
      artistName <- artistNameForReleaseRef release.id
      artist <- artistForRef artistName.artist_id
      pure (artist, artistName)
  getByName name = do
    RepositoryHandle {connSrc} <- ask
    runSelect connSrc $
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

runArtistRepositoryIO :: DbConnection -> ArtistRepositoryIOT m a -> m a
runArtistRepositoryIO connSrc =
  flip
    runReaderT
    RepositoryHandle
      { connSrc,
        tbl = artistSchema,
        pk = (.id),
        upsert = Nothing
      }
    . runRepositoryIOT
    . runArtistRepositoryIOT
