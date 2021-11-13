{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Album.Types
import Melo.Library.Artist.Types
import Melo.Library.Genre.Types
import Melo.Library.Track.Types
import Rel8
import Witch

class Repository (AlbumTable Result) m => AlbumRepository m where
  getAlbumGenres :: AlbumRef -> m [Genre]
  getAlbumArtists :: AlbumRef -> m [Artist]
  getAlbumTracks :: AlbumRef -> m [Track]
  searchAlbums :: Text -> m [Album]

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
      Repository (AlbumTable Result)
    )

instance
  ( MonadIO m
  ) =>
  AlbumRepository (AlbumRepositoryIOT m)
  where
  getAlbumGenres k = error "unimplemented"
  getAlbumArtists k = error "unimplemented"
  getAlbumTracks k = error "unimplemented"
  searchAlbums k = error "unimplemented"

--runAlbumRepositoryIO :: Connection -> AlbumRepositoryIOT m a -> m a
--runAlbumRepositoryIO conn = flip runReaderT conn . runAlbumRepositoryIOT
