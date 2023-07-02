{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Monad.Reader
import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Source.Types
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz as MB
import Rel8

class Repository TrackEntity m => TrackRepository m where
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe TrackEntity)
  getBySrcRef :: SourceRef -> m (Maybe TrackEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TrackRepository m
  ) =>
  TrackRepository (t m)
  where
  getByMusicBrainzId = lift . getByMusicBrainzId
  getBySrcRef = lift . getBySrcRef

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
  getBySrcRef srcId = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
      track <- Rel8.each tbl
      Rel8.where_ $ track.source_id ==. lit srcId
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
            release_id = "release_id",
            track_number = "track_number",
            disc_number = "disc_number",
            comment = "comment",
            length = "length",
            source_id = "source_id",
            musicbrainz_id = "musicbrainz_id"
          }
    }

runTrackRepositoryIO :: DbConnection -> TrackRepositoryIOT m a -> m a
runTrackRepositoryIO connSrc =
  flip
    runReaderT
    RepositoryHandle
      { connSrc,
        tbl = trackSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = (.source_id),
                set = const,
                updateWhere = \_new _old -> lit True
              }
      }
    . runRepositoryIOT
    . runTrackRepositoryIOT
