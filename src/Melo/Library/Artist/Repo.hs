{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

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

data NewArtist
  = NewArtist
      {
      }
  deriving (Generic, Eq, Show)

data ArtistRepository m k
  = GetAllArtists ([DB.Artist] -> m k)
  | GetArtists [DB.ArtistKey] ([DB.Artist] -> m k)
  | GetArtistAlbums DB.ArtistKey ([DB.Album] -> m k)
  | GetArtistTracks DB.ArtistKey ([DB.Track] -> m k)
  | SearchArtists Text ([DB.Artist] -> m k)
  | InsertArtists [NewArtist] ([DB.ArtistKey] -> m k)
  | DeleteArtists [DB.ArtistKey] (m k)
  deriving (Functor, Generic1, HFunctor, Effect)

getAllArtists :: Has ArtistRepository sig m => m [DB.Artist]
getAllArtists = send (GetAllArtists pure)

getArtists :: Has ArtistRepository sig m => [DB.ArtistKey] -> m [DB.Artist]
getArtists ks = send (GetArtists ks pure)

getArtistAlbums :: Has ArtistRepository sig m => DB.ArtistKey -> m [DB.Album]
getArtistAlbums k = send (GetArtistAlbums k pure)

getArtistTracks :: Has ArtistRepository sig m => DB.ArtistKey -> m [DB.Track]
getArtistTracks k = send (GetArtistTracks k pure)

searchArtists :: Has ArtistRepository sig m => Text -> m [DB.Artist]
searchArtists t = send (SearchArtists t pure)

insertArtists :: Has ArtistRepository sig m => [NewArtist] -> m [DB.ArtistKey]
insertArtists as = send (InsertArtists as pure)

deleteArtists :: Has ArtistRepository sig m => [DB.ArtistKey] -> m ()
deleteArtists ks = send (DeleteArtists ks (pure ()))

newtype ArtistRepositoryIOC m a
  = ArtistRepositoryIOC
      { runArtistRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig) =>
  Algebra (ArtistRepository :+: sig) (ArtistRepositoryIOC m)
  where
  alg = \case
    R other -> ArtistRepositoryIOC (alg (R (handleCoercible other)))

runArtistRepositoryIO :: Connection -> ArtistRepositoryIOC m a -> m a
runArtistRepositoryIO conn = runReader conn . runArtistRepositoryIOC
