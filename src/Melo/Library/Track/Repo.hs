{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens (firstOf, (^.))
import Control.Monad.Reader
import Data.Hashable
import Data.Pool
import Hasql.Connection
import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Album.Types
import Melo.Library.Artist.Types
import Melo.Library.Genre.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz as MB
import Rel8
import Witch

class Repository TrackEntity m => TrackRepository m where
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe TrackEntity)

--  getTrackSource :: UUID -> m Source
--  getTrackArtists :: UUID -> m [Artist]
--  getTrackAlbum :: UUID -> m Album
--  getTrackGenres :: UUID -> m [Genre]
--  searchTracks :: Text -> m [Track]

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TrackRepository m
  ) =>
  TrackRepository (t m)
  where
  getByMusicBrainzId = lift . getByMusicBrainzId

newtype TrackRepositoryIOT m a = TrackRepositoryIOT
  { runTrackRepositoryIOT :: RepositoryIOT TrackTable m a
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
      MonadReader (RepositoryHandle TrackTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad,
      Repository (TrackTable Result)
    )

instance
  ( MonadIO m
  ) =>
  TrackRepository (TrackRepositoryIOT m)
  where
  getByMusicBrainzId mbid = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
      track <- Rel8.each tbl
      Rel8.where_ $ track.musicbrainz_id ==. lit (Just mbid.mbid)
      pure track

trackForRef :: Expr TrackRef -> Query (TrackTable Expr)
trackForRef trackRef =
  Rel8.filter (\track -> track.id ==. trackRef) =<< each trackSchema

trackForSourceRef :: Expr SourceRef -> Query (TrackTable Expr)
trackForSourceRef srcRef =
  Rel8.filter (\track -> track.source_id ==. srcRef) =<< each trackSchema

trackSchema :: TableSchema (TrackTable Name)
trackSchema =
  TableSchema
    { name = "track",
      schema = Nothing,
      columns =
        TrackTable
          { id = "id",
            title = "title",
            album_id = "album_id",
            track_number = "track_number",
            disc_number = "disc_number",
            comment = "comment",
            length = "length",
            source_id = "source_id",
            musicbrainz_id = "musicbrainz_id"
          }
    }

runTrackRepositoryPooledIO :: Pool Connection -> TrackRepositoryIOT m a -> m a
runTrackRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = trackSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = \e -> (e.track_number, e.disc_number, e.album_id),
                set = const,
                updateWhere = \new old ->
                  new.track_number ==. old.track_number
                    &&. new.disc_number ==. old.disc_number
                    &&. new.album_id ==. old.album_id
              }
      }
    . runRepositoryIOT
    . runTrackRepositoryIOT

runTrackRepositoryIO :: Connection -> TrackRepositoryIOT m a -> m a
runTrackRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = trackSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = \e -> (e.track_number, e.disc_number, e.album_id),
                set = const,
                updateWhere = \new old ->
                  new.track_number ==. old.track_number
                    &&. new.disc_number ==. old.disc_number
                    &&. new.album_id ==. old.album_id
              }
      }
    . runRepositoryIOT
    . runTrackRepositoryIOT
