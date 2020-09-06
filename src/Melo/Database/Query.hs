module Melo.Database.Query where

import Control.Effect.Lift
import Control.Effect.Reader
import Database.Beam
--import Database.Beam.Query.Internal
import Database.Beam.Postgres
import qualified Language.Haskell.TH.Syntax as TH (Exp, Q)
import Melo.Common.Logging
import Melo.Database.Model

type QL = Q Postgres MeloDb

type QPgExpr = QExpr Postgres

--type QPgAgg = QAgg Postgres

type ReusableQL = ReusableQ Postgres MeloDb

type WithL = With Postgres MeloDb

runPgDebug ::
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m
  ) =>
  Pg a ->
  m a
runPgDebug q = do
  conn <- ask
  sendIO $ runBeamPostgresDebug (runStdoutLogging . $(logDebug)) conn q

runPgDebug' :: TH.Q TH.Exp
runPgDebug' =
  [|(\q -> do conn <- ask; sendIO $ runBeamPostgresDebug (runStdoutLogging . $(logDebug)) conn q)|]

getAll ::
  ( Has (Reader Connection) sig m,
    Has (Lift IO) sig m,
    Table tbl,
    FromBackendRow Postgres (tbl Identity)
  ) =>
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
  m [tbl Identity]
getAll tbl = runPgDebug (runSelectReturningList (selectAll tbl))

getAllSorted o tbl = runPgDebug (runSelectReturningList (selectAllSorted o tbl))

selectAll ::
  Table tbl =>
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
  SqlSelect Postgres (tbl Identity)
selectAll = select . all_

--selectAllSorted ::
--  Table tbl =>
--  orderer ->
--  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
--  SqlSelect Postgres (tbl Identity)
selectAllSorted o = select . orderBy_ (asc_ . o) . all_

getByKeys ::
  ( Has (Reader Connection) sig m,
    Has (Lift IO) sig m,
    Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl),
    FromBackendRow Postgres (tbl Identity)
  ) =>
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  m [tbl Identity]
getByKeys _ [] = pure []
getByKeys tbl ks =
  let q = select (byKeys tbl ks)
   in runPgDebug (runSelectReturningList q)

byKeys ::
  ( Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl)
  ) =>
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
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
deleteByKeys tbl ks =
  let q = delete tbl (\t -> primaryKey t `in_` (val_ <$> ks))
   in runPgDebug (runDelete q)
