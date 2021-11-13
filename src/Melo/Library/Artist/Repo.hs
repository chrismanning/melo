{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

import Control.Exception.Safe
import Control.Lens ((^.))
import Control.Concurrent.Classy
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Containers.ListUtils (nubOrd)
import Data.Pool
import Data.Text (Text)
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Types
import Rel8
import Witch

class Repository (ArtistTable Result) m => ArtistRepository m where
  getArtistAlbums :: ArtistRef -> m [ArtistTable Result]
  getArtistTracks :: ArtistRef -> m [ArtistTable Result]
  searchArtists :: Text -> m [ArtistTable Result]

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
      Repository (ArtistTable Result)
    )

instance
  ( MonadIO m
  ) =>
  ArtistRepository (ArtistRepositoryIOT m)
  where
  getArtistAlbums k = error "unimplemented"
  getArtistTracks k = error "unimplemented"
  searchArtists t = error "unimplemented"

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

runArtistRepositoryPooledIO :: Pool Connection -> ArtistRepositoryIOT m a -> m a
runArtistRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = artistSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = \c -> (c ^. #name, c ^. #disambiguation),
                set = const,
                updateWhere = \new old -> new ^. #id ==. old ^. #id
              }
      }
    . runRepositoryIOT
    . runArtistRepositoryIOT

runArtistRepositoryIO :: Connection -> ArtistRepositoryIOT m a -> m a
runArtistRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = artistSchema,
        pk = (^. #id),
        upsert =
          Just
            Upsert
              { index = \c -> (c ^. #name, c ^. #disambiguation),
                set = const,
                updateWhere = \new old -> new ^. #id ==. old ^. #id
              }
      }
    . runRepositoryIOT
    . runArtistRepositoryIOT
