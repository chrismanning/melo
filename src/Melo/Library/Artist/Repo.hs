{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Artist.Repo where

import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens ((^.), firstOf)
import Control.Concurrent.Classy
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Pool
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Rel8

class Repository ArtistEntity m => ArtistRepository m where
--  getArtistAlbums :: ArtistRef -> m [ArtistTable Result]
--  getArtistTracks :: ArtistRef -> m [ArtistTable Result]
--  searchArtists :: Text -> m [ArtistTable Result]
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe ArtistEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ArtistRepository m
  ) =>
  ArtistRepository (t m)
  where
--  getArtistAlbums = lift . getArtistAlbums
--  getArtistTracks = lift . getArtistTracks
--  searchArtists = lift . searchArtists
  getByMusicBrainzId = lift . getByMusicBrainzId

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
      PrimMonad,
      Repository ArtistEntity
    )

instance
  ( MonadIO m
  ) =>
  ArtistRepository (ArtistRepositoryIOT m)
  where
  getByMusicBrainzId mbid = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
      artist <- Rel8.each tbl
      Rel8.where_ $ artist.musicbrainz_id ==. lit (Just $ coerce mbid)
      pure artist

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
