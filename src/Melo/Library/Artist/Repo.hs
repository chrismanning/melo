{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens ((^.))
import Country
import Data.Functor
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import Melo.Common.Effect
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewArtist
  = NewArtist
      { name :: Text,
        country :: Maybe Country,
        bio :: Maybe Text,
        shortBio :: Maybe Text
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
      { runArtistRepositoryIOC :: m a
      }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m
  ) =>
  Algebra (ArtistRepository :+: sig) (ArtistRepositoryIOC m)
  where
  alg hdl (L sig) ctx = case sig of
    GetAllArtists -> undefined
    GetArtists ks -> undefined
    GetArtistAlbums k -> undefined
    GetArtistTracks k -> undefined
    SearchArtists t -> undefined
    InsertArtists as -> ArtistRepositoryIOC $ do
      conn <- ask
      let q =
            runPgInsertReturningList $
              insertReturning
                (DB.libraryDb ^. #artist)
                ( insertExpressions
                    (fmap newArtist as)
                )
                onConflictDefault
                (Just primaryKey)
      (ctx $>) <$> runArtistRepositoryIOC ($(runPgDebug') conn q)
    DeleteArtists ks -> undefined
  alg hdl (R other) ctx = ArtistRepositoryIOC (alg (runArtistRepositoryIOC . hdl) other ctx)

newArtist :: NewArtist -> DB.ArtistT (QExpr Postgres s)
newArtist a =
  DB.Artist
    { id = default_,
      name = val_ (a ^. #name),
      country = val_ (alphaThreeLower <$> a ^. #country),
      bio = val_ (a ^. #bio),
      short_bio = val_ (a ^. #shortBio)
    }

runArtistRepositoryIO :: ArtistRepositoryIOC m a -> m a
runArtistRepositoryIO = runArtistRepositoryIOC
