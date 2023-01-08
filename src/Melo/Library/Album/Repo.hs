{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens hiding (from)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Album.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Rel8 (Expr, Query, lit, (==.), (||.))
import Rel8 qualified
import Witch

class Repository AlbumEntity m => AlbumRepository m where
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe AlbumEntity)

--  getAlbumGenres :: AlbumRef -> m [Genre]
--  getAlbumTracks :: AlbumRef -> m [Track]
--  searchAlbums :: Text -> m [Album]

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    AlbumRepository m
  ) =>
  AlbumRepository (t m)
  where
  getByMusicBrainzId = lift . getByMusicBrainzId

newtype AlbumRepositoryIOT m a = AlbumRepositoryIOT
  { runAlbumRepositoryIOT :: RepositoryIOT AlbumTable m a
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
      MonadReader (RepositoryHandle AlbumTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance MonadIO m => Repository AlbumEntity (AlbumRepositoryIOT m) where
  getAll = AlbumRepositoryIOT getAll
  getByKey = AlbumRepositoryIOT . getByKey
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
  delete = AlbumRepositoryIOT . delete @AlbumEntity
  update = AlbumRepositoryIOT . update
  update' = AlbumRepositoryIOT . update'

instance
  ( MonadIO m
  ) =>
  AlbumRepository (AlbumRepositoryIOT m)
  where
  getByMusicBrainzId mbid = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
      album <- Rel8.each tbl
      let mbid' = lit (Just mbid.mbid)
      Rel8.where_ $
        album.musicbrainz_id ==. mbid' ||. album.musicbrainz_group_id ==. mbid'
      pure album

albumForRef :: Expr AlbumRef -> Query (AlbumTable Expr)
albumForRef albumRef = Rel8.filter (\album -> album.id ==. albumRef) =<< Rel8.each albumSchema

albumSchema :: Rel8.TableSchema (AlbumTable Rel8.Name)
albumSchema =
  Rel8.TableSchema
    { name = "album",
      schema = Nothing,
      columns =
        AlbumTable
          { id = "id",
            title = "title",
            comment = "comment",
            year_released = "year_released",
            original_year_released = "original_year_released",
            length = "length",
            musicbrainz_id = "musicbrainz_id",
            musicbrainz_group_id = "musicbrainz_group_id",
            catalogue_number = "catalogue_number"
          }
    }

runAlbumRepositoryPooledIO :: Pool Connection -> AlbumRepositoryIOT m a -> m a
runAlbumRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = albumSchema,
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
    . runAlbumRepositoryIOT

runAlbumRepositoryIO :: Connection -> AlbumRepositoryIOT m a -> m a
runAlbumRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = albumSchema,
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
    . runAlbumRepositoryIOT
