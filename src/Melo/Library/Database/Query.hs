module Melo.Library.Database.Query where

import Control.Effect.Lift
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Query.Internal
import qualified Language.Haskell.TH.Syntax as TH (Exp, Q)
import Melo.Common.Logging
import Melo.Library.Database.Model

type QL = Q Postgres LibraryDb

type QPgExpr = QExpr Postgres

type QPgAgg = QAgg Postgres

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
