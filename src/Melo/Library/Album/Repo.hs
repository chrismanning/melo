{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Repo where

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens ((^.))
import Data.Functor
import Data.List
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import Melo.Library.Album.Types
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data AlbumRepository :: Effect where
  GetAllAlbums :: AlbumRepository m [DB.Album]
  GetAlbums :: [DB.AlbumKey] -> AlbumRepository m [DB.Album]
  GetAlbumGenres :: DB.AlbumKey -> AlbumRepository m [DB.Genre]
  GetAlbumArtists :: DB.AlbumKey -> AlbumRepository m [DB.Artist]
  GetAlbumTracks :: DB.AlbumKey -> AlbumRepository m [DB.Track]
  SearchAlbums :: Text -> AlbumRepository m [DB.Album]
  InsertAlbums :: [NewAlbum] -> AlbumRepository m [DB.AlbumKey]
  DeleteAlbums :: [DB.AlbumKey] -> AlbumRepository m ()

getAllAlbums :: Has AlbumRepository sig m => m [DB.Album]
getAllAlbums = send GetAllAlbums

getAlbums :: Has AlbumRepository sig m => [DB.AlbumKey] -> m [DB.Album]
getAlbums ks = send (GetAlbums ks)

getAlbumGenres :: Has AlbumRepository sig m => DB.AlbumKey -> m [DB.Genre]
getAlbumGenres k = send (GetAlbumGenres k)

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

newtype AlbumRepositoryIOC m a = AlbumRepositoryIOC
  { runAlbumRepositoryIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

tbl :: DatabaseEntity Postgres DB.LibraryDb (TableEntity DB.AlbumT)
tbl = DB.libraryDb ^. #album

instance
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m
  ) =>
  Algebra (AlbumRepository :+: sig) (AlbumRepositoryIOC m)
  where
  alg _ (L sig) ctx = case sig of
    GetAllAlbums -> ctx $$> getAll tbl
    GetAlbums ks -> ctx $$> getByKeys tbl ks
    DeleteAlbums ks -> ctx $$> deleteByKeys tbl ks
    GetAlbumGenres k -> undefined
    GetAlbumArtists k -> undefined
    GetAlbumTracks k -> undefined
    SearchAlbums k -> undefined
    InsertAlbums as' -> do
      conn <- ask
      let !as = nub as'
      let q =
            runPgInsertReturningList $
              insertReturning
                (DB.libraryDb ^. #album)
                ( insertExpressions
                    (fmap from as)
                )
                Pg.onConflictDefault
                (Just primaryKey)
      ctx $$> $(runPgDebug') conn q
  alg hdl (R other) ctx = AlbumRepositoryIOC (alg (runAlbumRepositoryIOC . hdl) other ctx)

runAlbumRepositoryIO :: AlbumRepositoryIOC m a -> m a
runAlbumRepositoryIO = runAlbumRepositoryIOC
