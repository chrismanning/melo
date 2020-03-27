{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

import Control.Algebra
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import Melo.Common.Effect
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewArtist
  = NewArtist
      {
      }
  deriving (Generic, Eq, Show)

data ArtistRepository :: Effect where
  GetAllArtists :: ArtistRepository m [DB.Artist]
  GetArtists :: [DB.ArtistKey] -> ArtistRepository m [DB.Artist]
  GetArtistAlbums :: DB.ArtistKey -> ArtistRepository m [DB.Album]
  GetArtistTracks :: DB.ArtistKey -> ArtistRepository m [DB.Track]
  SearchArtists :: Text -> ArtistRepository m [DB.Artist]
  InsertArtists :: [NewArtist] -> ArtistRepository m [DB.ArtistKey]
  DeleteArtists :: [DB.ArtistKey] -> ArtistRepository m ()

getAllArtists :: Has ArtistRepository sig m => m [DB.Artist]
getAllArtists = send GetAllArtists

getArtists :: Has ArtistRepository sig m => [DB.ArtistKey] -> m [DB.Artist]
getArtists ks = send (GetArtists ks)

getArtistAlbums :: Has ArtistRepository sig m => DB.ArtistKey -> m [DB.Album]
getArtistAlbums k = send (GetArtistAlbums k)

getArtistTracks :: Has ArtistRepository sig m => DB.ArtistKey -> m [DB.Track]
getArtistTracks k = send (GetArtistTracks k)

searchArtists :: Has ArtistRepository sig m => Text -> m [DB.Artist]
searchArtists t = send (SearchArtists t)

insertArtists :: Has ArtistRepository sig m => [NewArtist] -> m [DB.ArtistKey]
insertArtists as = send (InsertArtists as)

deleteArtists :: Has ArtistRepository sig m => [DB.ArtistKey] -> m ()
deleteArtists ks = send (DeleteArtists ks)

newtype ArtistRepositoryIOC m a
  = ArtistRepositoryIOC
      { runArtistRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m) =>
  Algebra (ArtistRepository :+: sig) (ArtistRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    R other -> ArtistRepositoryIOC (alg (runArtistRepositoryIOC . hdl) (R other) ctx)

runArtistRepositoryIO :: Connection -> ArtistRepositoryIOC m a -> m a
runArtistRepositoryIO conn = runReader conn . runArtistRepositoryIOC
