{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Album.Repo where

import Basement.From
import Control.Lens ((^.))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Album.Types

class Monad m => AlbumRepository m where
  getAllAlbums :: m [DB.Album]
  getAlbums :: [DB.AlbumKey] -> m [DB.Album]
  getAlbumGenres :: DB.AlbumKey -> m [DB.Genre]
  getAlbumArtists :: DB.AlbumKey -> m [DB.Artist]
  getAlbumTracks :: DB.AlbumKey -> m [DB.Track]
  searchAlbums :: Text -> m [DB.Album]
  insertAlbums :: [NewAlbum] -> m [DB.AlbumKey]
  deleteAlbums :: [DB.AlbumKey] -> m ()

newtype AlbumRepositoryIOT m a = AlbumRepositoryIOT
  { runAlbumRepositoryIOT :: ReaderT Connection m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans, MonadTransControl)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.AlbumT)
tbl = DB.meloDb ^. #album

instance
  ( MonadIO m
  ) =>
  AlbumRepository (AlbumRepositoryIOT m)
  where
  getAllAlbums = AlbumRepositoryIOT $
    ReaderT $ \conn ->
      getAllIO conn tbl
  getAlbums ks = AlbumRepositoryIOT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  getAlbumGenres k = error "unimplemented"
  getAlbumArtists k = error "unimplemented"
  getAlbumTracks k = error "unimplemented"
  deleteAlbums ks = AlbumRepositoryIOT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  searchAlbums k = error "unimplemented"
  insertAlbums as' = AlbumRepositoryIOT $
    ReaderT $ \conn -> do
      let !as = nubOrd as'
      let q =
            runPgInsertReturningList $
              insertReturning
                (DB.meloDb ^. #album)
                ( insertExpressions
                    (fmap from as)
                )
                Pg.onConflictDefault
                (Just primaryKey)
      $(runPgDebugIO') conn q

runAlbumRepositoryIO :: Connection -> AlbumRepositoryIOT m a -> m a
runAlbumRepositoryIO conn = flip runReaderT conn . runAlbumRepositoryIOT
