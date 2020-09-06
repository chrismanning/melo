{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens ((^.))
import Data.List
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Artist.Types

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

newtype ArtistRepositoryIOC m a = ArtistRepositoryIOC
  { runArtistRepositoryIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.ArtistT)
tbl = DB.meloDb ^. #artist

instance
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m
  ) =>
  Algebra (ArtistRepository :+: sig) (ArtistRepositoryIOC m)
  where
  alg _ (L sig) ctx = case sig of
    GetAllArtists -> ctx $$> getAll tbl
    GetArtists ks -> ctx $$> getByKeys tbl ks
    DeleteArtists ks -> ctx $$> deleteByKeys tbl ks
    GetArtistAlbums k -> undefined
    GetArtistTracks k -> undefined
    SearchArtists t -> undefined
    InsertArtists as' -> do
      let !as = nub as'
      let q =
            runPgInsertReturningList $
              insertReturning
                (DB.meloDb ^. #artist)
                ( insertExpressions
                    (fmap from as)
                )
                ( Pg.onConflict
                    (Pg.conflictingFields (\a -> (a ^. #name, a ^. #country, a ^. #disambiguation)))
                    ( Pg.onConflictUpdateInstead
                        (\a -> (a ^. #bio, a ^. #short_bio))
                    )
                )
                (Just primaryKey)
      ctx $$> $(runPgDebug') q
  alg hdl (R other) ctx = ArtistRepositoryIOC (alg (runArtistRepositoryIOC . hdl) other ctx)

runArtistRepositoryIO :: ArtistRepositoryIOC m a -> m a
runArtistRepositoryIO = runArtistRepositoryIOC
