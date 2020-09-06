{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Staging.Repo where

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens hiding (from)
import Data.List (nub)
import Data.Text
import Database.Beam
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Artist.Types

data ArtistStagingRepository :: Effect where
  GetAllStagedArtists :: ArtistStagingRepository m [DB.ArtistStage]
  GetStagedArtists :: [DB.ArtistStageKey] -> ArtistStagingRepository m [DB.ArtistStage]
  SearchStagedArtists :: Text -> ArtistStagingRepository m [DB.ArtistStage]
  InsertStagedArtists :: [NewStagedArtist] -> ArtistStagingRepository m [DB.ArtistStageKey]
  DeleteStagedArtists :: [DB.ArtistStageKey] -> ArtistStagingRepository m ()

getAllStagedArtists :: Has ArtistStagingRepository sig m => m [DB.ArtistStage]
getAllStagedArtists = send GetAllStagedArtists

getStagedArtists :: Has ArtistStagingRepository sig m => [DB.ArtistStageKey] -> m [DB.ArtistStage]
getStagedArtists ks = send (GetStagedArtists ks)

searchStagedArtists :: Has ArtistStagingRepository sig m => Text -> m [DB.ArtistStage]
searchStagedArtists t = send (SearchStagedArtists t)

insertStagedArtists :: Has ArtistStagingRepository sig m => [NewStagedArtist] -> m [DB.ArtistStageKey]
insertStagedArtists as = send (InsertStagedArtists as)

deleteStagedArtists :: Has ArtistStagingRepository sig m => [DB.ArtistStageKey] -> m ()
deleteStagedArtists ks = send (DeleteStagedArtists ks)

newtype ArtistStagingRepositoryIOC m a = ArtistStagingRepositoryIOC
  { runArtistStagingRepositoryIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.ArtistStageT)
tbl = DB.meloDb ^. #artist_stage

instance
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m
  ) =>
  Algebra (ArtistStagingRepository :+: sig) (ArtistStagingRepositoryIOC m)
  where
  alg _ (L sig) ctx = case sig of
    GetAllStagedArtists -> ctx $$> getAll tbl
    GetStagedArtists ks -> ctx $$> getByKeys tbl ks
    DeleteStagedArtists ks -> ctx $$> deleteByKeys tbl ks
    SearchStagedArtists t -> undefined
    InsertStagedArtists as' -> do
      let !as = nub as'
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
      ctx $$> $(runPgDebug') q
  alg hdl (R other) ctx = ArtistStagingRepositoryIOC (alg (runArtistStagingRepositoryIOC . hdl) other ctx)

runArtistStagingRepositoryIO :: ArtistStagingRepositoryIOC m a -> m a
runArtistStagingRepositoryIO = runArtistStagingRepositoryIOC
