module Melo.Library.Database.Query where

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Query.Internal
import Melo.Library.Database.Model

type QL = Q Postgres LibraryDb

type QPgExpr = QExpr Postgres

type QPgAgg = QAgg Postgres

type ReusableQL = ReusableQ Postgres LibraryDb

type WithL = With Postgres LibraryDb

runPgDebug :: MonadIO m => Connection -> Pg a -> m a
runPgDebug conn q = liftIO $ runBeamPostgresDebug putStrLn conn q
