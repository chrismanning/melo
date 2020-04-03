{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Repo where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import Melo.Common.Effect
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewAlbum
  = NewAlbum
      {
      }
  deriving (Generic, Eq, Show)

data AlbumRepository :: Effect where
  GetAllAlbums :: AlbumRepository m [DB.Album]
  GetAlbums :: [DB.AlbumKey] -> AlbumRepository m [DB.Album]
  GetAlbumArtists :: DB.AlbumKey -> AlbumRepository m [DB.Artist]
  GetAlbumTracks :: DB.AlbumKey -> AlbumRepository m [DB.Track]
  SearchAlbums :: Text -> AlbumRepository m [DB.Album]
  InsertAlbums :: [NewAlbum] -> AlbumRepository m [DB.AlbumKey]
  DeleteAlbums :: [DB.AlbumKey] -> AlbumRepository m ()

getAllAlbums :: Has AlbumRepository sig m => m [DB.Album]
getAllAlbums = send GetAllAlbums

getAlbums :: Has AlbumRepository sig m => [DB.AlbumKey] -> m [DB.Album]
getAlbums ks = send (GetAlbums ks)

getAlbumArtists :: Has AlbumRepository sig m => DB.AlbumKey -> m [DB.Artist]
getAlbumArtists k = send (GetAlbumArtists k)

getAlbumTracks :: Has AlbumRepository sig m => DB.AlbumKey -> m [DB.Track]
getAlbumTracks k = send (GetAlbumTracks k)

searchAlbums :: Has AlbumRepository sig m => Text -> m [DB.Album]
searchAlbums t = send (SearchAlbums t)

insertAlbums :: Has AlbumRepository sig m => [NewAlbum] -> m [DB.AlbumKey]
insertAlbums as = send (InsertAlbums as)

deleteAlbums :: Has AlbumRepository sig m => [DB.AlbumKey] -> m ()
deleteAlbums ks = send (DeleteAlbums ks)

newtype AlbumRepositoryIOC m a
  = AlbumRepositoryIOC
      { runAlbumRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad)

instance
  (Has (Lift IO) sig m) =>
  Algebra (AlbumRepository :+: sig) (AlbumRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    R other -> AlbumRepositoryIOC (alg (runAlbumRepositoryIOC . hdl) (R other) ctx)

runAlbumRepositoryIO :: Connection -> AlbumRepositoryIOC m a -> m a
runAlbumRepositoryIO conn = runReader conn . runAlbumRepositoryIOC
