{-# LANGUAGE QuantifiedConstraints #-}

module Melo.Database.Query where

import Control.Lens
import Control.Monad.IO.Class
import Data.String (IsString)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Schema.Tables
import qualified Language.Haskell.TH.Syntax as TH (Exp, Q)
import Melo.Common.Logging
import Melo.Database.Model

type QL = Q Postgres MeloDb

type QPgExpr = QExpr Postgres

--type QPgAgg = QAgg Postgres

type ReusableQL = ReusableQ Postgres MeloDb

type WithL = With Postgres MeloDb

runPgDebugIO ::
  (MonadIO m) =>
  Connection ->
  Pg a ->
  m a
runPgDebugIO conn q = liftIO do
  $(logDebugIO) ("before query" :: Text)
  !r <- runBeamPostgresDebug $(logDebugIO) conn q
  $(logDebugIO) ("after query" :: Text)
  pure r

runPgDebugIO' :: TH.Q TH.Exp
runPgDebugIO' =
  [|(\conn q -> liftIO $ runBeamPostgresDebug (runStdoutLogging . $(logDebug)) conn q)|]

getAllIO ::
  ( MonadIO m,
    Table tbl,
    FromBackendRow Postgres (tbl Identity)
  ) =>
  Connection ->
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
  m [tbl Identity]
getAllIO conn tbl = runPgDebugIO conn (runSelectReturningList (selectAll tbl))

getAllSortedIO conn o tbl = runPgDebugIO conn (runSelectReturningList (selectAllSorted o tbl))

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

getByKeysIO ::
  ( MonadIO m,
    Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl),
    FromBackendRow Postgres (tbl Identity)
  ) =>
  Connection ->
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  m [tbl Identity]
getByKeysIO _ _ [] = pure []
getByKeysIO conn tbl ks =
  let q = select (byKeys tbl ks)
   in runPgDebugIO conn (runSelectReturningList q)

byKeys ::
  ( Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl)
  ) =>
  DatabaseEntity Postgres MeloDb (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  QL s (tbl (QPgExpr s))
byKeys tbl ks = filter_ (\g -> primaryKey g `in_` (val_ <$> ks)) (all_ tbl)

deleteByKeysIO ::
  ( MonadIO m,
    Table tbl,
    SqlValableTable Postgres (PrimaryKey tbl)
  ) =>
  Connection ->
  DatabaseEntity Postgres db (TableEntity tbl) ->
  [PrimaryKey tbl Identity] ->
  m ()
deleteByKeysIO _ _ [] = pure ()
deleteByKeysIO conn tbl ks =
  let q = delete tbl (\t -> primaryKey t `in_` (val_ <$> ks))
   in runPgDebugIO conn (runDelete q)

deleteAllIO ::
  forall m tbl db.
  ( MonadIO m,
    Table tbl
  ) =>
  Connection ->
  DatabaseEntity Postgres db (TableEntity tbl) ->
  m ()
deleteAllIO conn tbl =
  let q = delete tbl (const $ val_ True)
   in do
        $(logInfoIO) $ "deleting all " <> tbl ^. dbEntityDescriptor . dbEntityName <> " entities"
        runPgDebugIO conn (runDelete q)

startsWith_ ::
  QPgExpr s text ->
  QPgExpr s text ->
  QPgExpr s Bool
startsWith_ = customExpr_ startsWithImpl
  where
    startsWithImpl :: (Monoid text, IsString text) => text -> text -> text
    startsWithImpl a b = "starts_with(" <> a <> ", " <> b <> ")"
