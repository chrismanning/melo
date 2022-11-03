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
import Data.Vector (Vector)
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Album.Repo (albumForRef)
import Melo.Library.Album.Types
import Melo.Library.Album.ArtistName.Repo
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Types
import Melo.Library.Source.Types (SourceRef)
import Melo.Library.Track.Repo (trackForSourceRef)
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Rel8

class Repository ArtistEntity m => ArtistRepository m where
--  getArtistAlbums :: ArtistRef -> m [ArtistTable Result]
--  getArtistTracks :: ArtistRef -> m [ArtistTable Result]
--  searchArtists :: Text -> m [ArtistTable Result]
  getByMusicBrainzId :: MB.MusicBrainzId -> m (Maybe ArtistEntity)
  getAlbumArtists :: AlbumRef -> m (Vector (ArtistEntity, ArtistNameEntity))
  getSourceAlbumArtists :: SourceRef -> m (Vector (ArtistEntity, ArtistNameEntity))

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
  getAlbumArtists = lift . getAlbumArtists
  getSourceAlbumArtists = lift . getSourceAlbumArtists

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
  getAlbumArtists albumRef = do
    RepositoryHandle {connSrc} <- ask
    runSelect connSrc do
      artistName <- artistNameForAlbumRef (lit albumRef)
      artist <- artistForRef artistName.artist_id
      pure (artist, artistName)
  getSourceAlbumArtists srcRef = do
    RepositoryHandle {connSrc} <- ask
    runSelect connSrc do
      track <- trackForSourceRef (lit srcRef)
      album <- albumForRef track.album_id
      artistName <- artistNameForAlbumRef album.id
      artist <- artistForRef artistName.artist_id
      pure (artist, artistName)

artistForRef :: Expr ArtistRef -> Query (ArtistTable Expr)
artistForRef artistRef =
  Rel8.filter (\artist -> artist.id ==. artistRef) =<< each artistSchema

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
