{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens ((^.))
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource
import Data.Containers.ListUtils (nubOrd)
import Data.Hashable
import Data.Int (Int16)
import Data.Pool
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Data.UUID
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Album.Types
import Melo.Library.Artist.Types
import Melo.Library.Genre.Types
import Melo.Library.Source.Types
import Melo.Library.Track.Types
import Rel8
import Witch

class Repository (TrackTable Result) m => TrackRepository m where
  getTrackSource :: UUID -> m Source
  getTrackArtists :: UUID -> m [Artist]
  getTrackAlbum :: UUID -> m Album
  getTrackGenres :: UUID -> m [Genre]
  searchTracks :: Text -> m [Track]

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
      MonadUnliftIO,
      Repository (TrackTable Result)
    )

instance
  ( MonadIO m
  ) =>
  TrackRepository (TrackRepositoryIOT m)
  where
  getTrackSource k = error "unimplemented"
  getTrackArtists k = error "unimplemented"
  getTrackAlbum k = error "unimplemented"
  getTrackGenres k = error "unimplemented"
  searchTracks t = error "unimplemented"

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
            source_id = "source_id"
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
              { index = (^. #id),
                set = const,
                updateWhere = \new old -> new ^. #id ==. old ^. #id
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
              { index = (^. #id),
                set = const,
                updateWhere = \new old -> new ^. #id ==. old ^. #id
              }
      }
    . runRepositoryIOT
    . runTrackRepositoryIOT
