{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Repo where

import Control.Algebra
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import GHC.Generics (Generic1)
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewAlbum
  = NewAlbum
      {
      }
  deriving (Generic, Eq, Show)

data AlbumRepository m k
  = GetAllAlbums ([DB.Album] -> m k)
  | GetAlbums [DB.AlbumKey] ([DB.Album] -> m k)
  | GetAlbumArtists DB.AlbumKey ([DB.Artist] -> m k)
  | GetAlbumTracks DB.AlbumKey ([DB.Track] -> m k)
  | SearchAlbums Text ([DB.Album] -> m k)
  | InsertAlbums [NewAlbum] ([DB.AlbumKey] -> m k)
  | DeleteAlbums [DB.AlbumKey] (m k)
  deriving (Functor, Generic1, HFunctor, Effect)

getAllAlbums :: Has AlbumRepository sig m => m [DB.Album]
getAllAlbums = send (GetAllAlbums pure)

getAlbums :: Has AlbumRepository sig m => [DB.AlbumKey] -> m [DB.Album]
getAlbums ks = send (GetAlbums ks pure)

getAlbumArtists :: Has AlbumRepository sig m => DB.AlbumKey -> m [DB.Artist]
getAlbumArtists k = send (GetAlbumArtists k pure)

getAlbumTracks :: Has AlbumRepository sig m => DB.AlbumKey -> m [DB.Track]
getAlbumTracks k = send (GetAlbumTracks k pure)

searchAlbums :: Has AlbumRepository sig m => Text -> m [DB.Album]
searchAlbums t = send (SearchAlbums t pure)

insertAlbums :: Has AlbumRepository sig m => [NewAlbum] -> m [DB.AlbumKey]
insertAlbums as = send (InsertAlbums as pure)

deleteAlbums :: Has AlbumRepository sig m => [DB.AlbumKey] -> m ()
deleteAlbums ks = send (DeleteAlbums ks (pure ()))

newtype AlbumRepositoryIOC m a
  = AlbumRepositoryIOC
      { runAlbumRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig) =>
  Algebra (AlbumRepository :+: sig) (AlbumRepositoryIOC m)
  where
  alg = \case
    R other -> AlbumRepositoryIOC (alg (R (handleCoercible other)))

runAlbumRepositoryIO :: Connection -> AlbumRepositoryIOC m a -> m a
runAlbumRepositoryIO conn = runReader conn . runAlbumRepositoryIOC
