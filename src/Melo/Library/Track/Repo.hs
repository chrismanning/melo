{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Source.Types
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz as MB
import Rel8

class (Repository TrackEntity m) => TrackRepository m where
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

instance TrackRepository (AppM IO IO) where
  getByMusicBrainzId mbid = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @TrackTable
    firstOf traverse <$> runSelect pool do
      track <- Rel8.each tbl
      Rel8.where_ $ track.musicbrainz_id ==. lit (Just mbid.mbid)
      pure track
  getBySrcRef srcId = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @TrackTable
    firstOf traverse <$> runSelect pool do
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

initTrackRepo :: (AppDataReader m) => m ()
initTrackRepo =
  putAppData
    RepositoryHandle
      { tbl = trackSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = (.source_id),
                predicate = Nothing,
                set = const,
                updateWhere = \_new _old -> lit True
              }
      }
