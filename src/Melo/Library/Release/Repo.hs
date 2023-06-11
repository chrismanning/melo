{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Release.Repo where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Foldl (PrimMonad)
import Control.Lens hiding (from)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Vector qualified as V
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Release.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Rel8 (Expr, Query, lit, (==.), (||.))
import Rel8 qualified
import Witch

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

newtype ReleaseRepositoryIOT m a = ReleaseRepositoryIOT
  { runReleaseRepositoryIOT :: RepositoryIOT ReleaseTable m a
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
      MonadReader (RepositoryHandle ReleaseTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance MonadIO m => Repository ReleaseEntity (ReleaseRepositoryIOT m) where
  getAll = ReleaseRepositoryIOT getAll
  getByKey = ReleaseRepositoryIOT . getByKey
  insert es | V.null es = pure V.empty
  insert es = do
    RepositoryHandle {connSrc, tbl} <- ask
    V.fromList
      <$> runInsert
        connSrc
        Rel8.Insert
          { into = tbl,
            rows = Rel8.values (from <$> es),
            onConflict = Rel8.DoNothing,
            returning = Rel8.Projection (\x -> x)
          }
  insert' es | V.null es = pure 0
  insert' es = do
    RepositoryHandle {connSrc, tbl} <- ask
    runInsert
      connSrc
      Rel8.Insert
        { into = tbl,
          rows = Rel8.values (from <$> es),
          onConflict = Rel8.DoNothing,
          returning = fromIntegral <$> Rel8.NumberOfRowsAffected
        }
  delete = ReleaseRepositoryIOT . delete @ReleaseEntity
  update = ReleaseRepositoryIOT . update
  update' = ReleaseRepositoryIOT . update'

instance
  ( MonadIO m
  ) =>
  ReleaseRepository (ReleaseRepositoryIOT m)
  where
  getByMusicBrainzId mbid = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
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

runReleaseRepositoryIO :: DbConnection -> ReleaseRepositoryIOT m a -> m a
runReleaseRepositoryIO connSrc =
  flip
    runReaderT
    RepositoryHandle
      { connSrc,
        tbl = releaseSchema,
        pk = (.id),
        upsert =
          Just
            Rel8.Upsert
              { index = (.musicbrainz_id),
                set = \new old -> new & #id .~ old.id,
                updateWhere = \new old -> new.musicbrainz_id ==. old.musicbrainz_id
              }
      }
    . runRepositoryIOT
    . runReleaseRepositoryIOT
