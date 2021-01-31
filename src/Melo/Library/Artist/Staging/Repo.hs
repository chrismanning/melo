{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Staging.Repo where

import Basement.From
import Control.Lens hiding (from)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Containers.ListUtils (nubOrd)
import Data.Text
import Database.Beam
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Artist.Types

class Monad m => ArtistStagingRepository m where
  getAllStagedArtists :: m [DB.ArtistStage]
  getStagedArtists :: [DB.ArtistStageKey] -> m [DB.ArtistStage]
  searchStagedArtists :: Text -> m [DB.ArtistStage]
  insertStagedArtists :: [NewStagedArtist] -> m [DB.ArtistStageKey]
  deleteStagedArtists :: [DB.ArtistStageKey] -> m ()

newtype ArtistStagingRepositoryT m a = ArtistStagingRepositoryT
  { runArtistStagingRepositoryT :: ReaderT Connection m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadTrans, MonadTransControl)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.ArtistStageT)
tbl = DB.meloDb ^. #artist_stage

instance
  ( MonadIO m
  ) =>
  ArtistStagingRepository (ArtistStagingRepositoryT m)
  where
  getAllStagedArtists = ArtistStagingRepositoryT $
    ReaderT $ \conn ->
      getAllIO conn tbl
  getStagedArtists ks = ArtistStagingRepositoryT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  deleteStagedArtists ks = ArtistStagingRepositoryT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  searchStagedArtists t = error "unimplemented"
  insertStagedArtists as' = ArtistStagingRepositoryT $
    ReaderT $ \conn -> do
      let !as = nubOrd as'
      let q =
            runPgInsertReturningList $
              insertReturning
                tbl
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

runArtistStagingRepositoryIO :: Connection -> ArtistStagingRepositoryT m a -> m a
runArtistStagingRepositoryIO conn = flip runReaderT conn . runArtistStagingRepositoryT
