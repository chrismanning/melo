{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Repo where

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
import Melo.Library.Artist.Types

class Monad m => ArtistRepository m where
  getAllArtists :: m [DB.Artist]
  getArtists :: [DB.ArtistKey] -> m [DB.Artist]
  getArtistAlbums :: DB.ArtistKey -> m [DB.Album]
  getArtistTracks :: DB.ArtistKey -> m [DB.Track]
  searchArtists :: Text -> m [DB.Artist]
  insertArtists :: [NewArtist] -> m [DB.ArtistKey]
  deleteArtists :: [DB.ArtistKey] -> m ()

newtype ArtistRepositoryT m a = ArtistRepositoryT
  { runArtistRepositoryT :: ReaderT Connection m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans, MonadTransControl)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.ArtistT)
tbl = DB.meloDb ^. #artist

instance
  ( MonadIO m
  ) =>
  ArtistRepository (ArtistRepositoryT m)
  where
  getAllArtists = ArtistRepositoryT $
    ReaderT $ \conn ->
      getAllIO conn tbl
  getArtists ks = ArtistRepositoryT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  deleteArtists ks = ArtistRepositoryT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  getArtistAlbums k = error "unimplemented"
  getArtistTracks k = error "unimplemented"
  searchArtists t = error "unimplemented"
  insertArtists as' = ArtistRepositoryT $
    ReaderT $ \conn -> do
      let !as = nubOrd as'
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
      $(runPgDebugIO') conn q

runArtistRepositoryIO :: Connection -> ArtistRepositoryT m a -> m a
runArtistRepositoryIO conn = flip runReaderT conn . runArtistRepositoryT
