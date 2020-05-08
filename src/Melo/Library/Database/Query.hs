module Melo.Library.Database.Query where

import Control.Effect.Lift
import Control.Effect.Reader
import Database.Beam
--import Database.Beam.Query.Internal
import Database.Beam.Postgres
import qualified Language.Haskell.TH.Syntax as TH (Exp, Q)
import Melo.Common.Logging
import Melo.Library.Database.Model

type QL = Q Postgres LibraryDb

type QPgExpr = QExpr Postgres

--type QPgAgg = QAgg Postgres

type ReusableQL = ReusableQ Postgres LibraryDb

type WithL = With Postgres LibraryDb

runPgDebug ::
  Has (Lift IO) sig m =>
  Connection ->
  Pg a ->
  m a
runPgDebug conn q = sendIO $ runBeamPostgresDebug (runStdoutLogging . $(logDebug)) conn q

runPgDebug' :: TH.Q TH.Exp
runPgDebug' =
  [|(\conn q -> sendIO $ runBeamPostgresDebug (runStdoutLogging . $(logDebug)) conn q)|]

getAll ::
  ( Has (Reader Connection) sig m,
    Has (Lift IO) sig m,
    Database Postgres db,
    Table tbl,
    FromBackendRow Postgres (tbl Identity)
  ) =>
  DatabaseEntity Postgres db (TableEntity tbl) ->
  m [tbl Identity]
getAll tbl = do
  conn <- ask
  let q = select (all_ tbl)
  runPgDebug conn (runSelectReturningList q)

getByKeys ::
  ( Has (Reader Connection) sig m,
    Has (Lift IO) sig m,
    Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl),
    FromBackendRow Postgres (tbl Identity)
  ) =>
  DatabaseEntity Postgres LibraryDb (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  m [tbl Identity]
getByKeys _ [] = pure []
getByKeys tbl ks = do
  conn <- ask
  let q = select (byKeys tbl ks)
  runPgDebug conn (runSelectReturningList q)

byKeys ::
  ( Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl)
  ) =>
  DatabaseEntity Postgres LibraryDb (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  QL s (tbl (QPgExpr s))
byKeys tbl ks = filter_ (\g -> primaryKey g `in_` (val_ <$> ks)) (all_ tbl)

deleteByKeys ::
  ( Has (Reader Connection) sig m,
    Has (Lift IO) sig m,
    Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl)
  ) =>
  DatabaseEntity Postgres db (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  m ()
deleteByKeys _ [] = pure ()
deleteByKeys tbl ks = do
  conn <- ask
  let q = delete tbl (\t -> primaryKey t `in_` (val_ <$> ks))
  runPgDebug conn (runDelete q)
