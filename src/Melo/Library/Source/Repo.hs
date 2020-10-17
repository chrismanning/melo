{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Repo where

import Basement.From
import Control.Algebra
import Control.Effect.Exception
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.TH
import Control.Lens hiding (from)
import Control.Monad
import Data.Functor
import qualified Data.Text as T
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import Melo.Common.NaturalSort
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Source.Types
import Network.URI

data SourceRepository :: Effect where
  GetAllSources :: SourceRepository m [DB.Source]
  GetSources :: [DB.SourceKey] -> SourceRepository m [DB.Source]
  GetSourcesByUri :: [URI] -> SourceRepository m [DB.Source]
  InsertSources :: [NewSource] -> SourceRepository m [DB.Source]
  DeleteSources :: [DB.SourceKey] -> SourceRepository m ()
  UpdateSources :: [UpdateSource] -> SourceRepository m ()

makeSmartConstructors ''SourceRepository

newtype SourceRepositoryIOC m a = SourceRepositoryIOC
  { runSourceRepositoryIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.SourceT)
tbl = DB.meloDb ^. #source

instance
  (Has (Lift IO) sig m, Has (Reader Connection) sig m) =>
  Algebra (SourceRepository :+: sig) (SourceRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    L GetAllSources -> ctx $$> sortNaturalBy (^. #source_uri) <$> getAll tbl
    L (GetSources ks) -> ctx $$> getByKeys tbl ks
    L (GetSourcesByUri []) -> pure $ ctx $> []
    L (GetSourcesByUri fs) -> SourceRepositoryIOC $ do
      let q = filter_ (\t -> t ^. #source_uri `in_` fmap (val_ . T.pack . show) fs) (all_ $ DB.meloDb ^. #source)
      r <- $(runPgDebug') (runSelectReturningList (select q))
      pure $ ctx $> r
    L (InsertSources []) -> pure $ ctx $> []
    L (InsertSources ss) -> do
      r <- do
        expr <- evaluate (insertExpressions $ fmap from ss)
        let q =
              Pg.insertReturning
                tbl
                expr
                ( Pg.onConflict
                    (B.conflictingFields (\t -> (t ^. #source_uri, t ^. #idx)))
                    ( Pg.onConflictUpdateInstead
                        (\s -> (s ^. #scanned, s ^. #metadata, s ^. #kind, s ^. #metadata_format))
                    )
                )
                (Just id)
        $(runPgDebug') (Pg.runPgInsertReturningList q)
      pure $ ctx $> r
    L (DeleteSources ks) -> ctx $$> deleteByKeys tbl ks
    L (UpdateSources us) -> do
      forM_ us $ \u -> do
        let q = save tbl u
        $(runPgDebug') (runUpdate q)
      pure ctx
    R other -> SourceRepositoryIOC (alg (runSourceRepositoryIOC . hdl) other ctx)

runSourceRepositoryIO :: SourceRepositoryIOC m a -> m a
runSourceRepositoryIO = runSourceRepositoryIOC
