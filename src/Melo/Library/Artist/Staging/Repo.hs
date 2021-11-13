{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Staging.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Containers.ListUtils (nubOrd)
import Data.Pool
import Data.Text
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Staging.Types
import Rel8

class Repository (ArtistStageTable Result) m => ArtistStagingRepository m where
  searchStagedArtists :: Text -> m [ArtistStageTable Result]

newtype ArtistStagingRepositoryIOT m a = ArtistStagingRepositoryIOT
  { runArtistStagingRepositoryIOT :: RepositoryIOT ArtistStageTable m a
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
      MonadReader (RepositoryHandle ArtistStageTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      Repository (ArtistStageTable Result)
    )

instance
  ( MonadIO m
  ) =>
  ArtistStagingRepository (ArtistStagingRepositoryIOT m)
  where
  searchStagedArtists t = error "unimplemented"

artistStageSchema :: TableSchema (ArtistStageTable Name)
artistStageSchema =
  TableSchema
    { name = "artist_stage",
      schema = Nothing,
      columns =
        ArtistStageTable
          { id = "id",
            name = "name",
            disambiguation = "disambiguation",
            short_bio = "short_bio",
            bio = "bio",
            country = "country",
            musicbrainz_id = "musicbrainz_id",
            ref_artist_id = "ref_artist_id",
            ref_album_id = "ref_album_id",
            ref_track_id = "ref_track_id"
          }
    }

runArtistStagingRepositoryPooledIO :: Pool Connection -> ArtistStagingRepositoryIOT m a -> m a
runArtistStagingRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = artistStageSchema,
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
    . runArtistStagingRepositoryIOT

runArtistStagingRepositoryIO :: Connection -> ArtistStagingRepositoryIOT m a -> m a
runArtistStagingRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = artistStageSchema,
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
    . runArtistStagingRepositoryIOT
