{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Repo where

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens hiding (from)
import Data.Functor
import qualified Data.Text as T
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query
import Melo.Library.Source.Types
import Network.URI

getSources :: Has SourceRepository sig m => [DB.SourceKey] -> m [DB.Source]
getSources ks = send (GetSources ks)

getSourcesByUri :: Has SourceRepository sig m => [URI] -> m [DB.Source]
getSourcesByUri srcs = send (GetSourcesByUri srcs)

insertSources :: Has SourceRepository sig m => [NewSource] -> m [DB.Source]
insertSources ts = send (InsertSources ts)

deleteSources :: Has SourceRepository sig m => [DB.SourceKey] -> m ()
deleteSources ks = send (DeleteSources ks)

data SourceRepository :: Effect where
  GetSources :: [DB.SourceKey] -> SourceRepository m [DB.Source]
  GetSourcesByUri :: [URI] -> SourceRepository m [DB.Source]
  InsertSources :: [NewSource] -> SourceRepository m [DB.Source]
  DeleteSources :: [DB.SourceKey] -> SourceRepository m ()

newtype SourceRepositoryIOC m a = SourceRepositoryIOC
  { runSourceRepositoryIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance
  (Has (Lift IO) sig m, Has (Reader Connection) sig m) =>
  Algebra (SourceRepository :+: sig) (SourceRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    L (GetSources []) -> pure $ ctx $> []
    L (GetSources ks) -> SourceRepositoryIOC $ do
      conn <- ask
      let ids = fmap (\(DB.SourceKey k') -> val_ k') ks
      let q = filter_ (\m -> m ^. #id `in_` ids) $ all_ (DB.libraryDb ^. #source)
      r <- runSourceRepositoryIOC ($(runPgDebug') conn (runSelectReturningList (select q)))
      pure $ ctx $> r
    L (GetSourcesByUri []) -> pure $ ctx $> []
    L (GetSourcesByUri fs) -> SourceRepositoryIOC $ do
      conn <- ask
      let q = filter_ (\t -> t ^. #source_uri `in_` fmap (val_ . T.pack . show) fs) (all_ $ DB.libraryDb ^. #source)
      r <- runSourceRepositoryIOC ($(runPgDebug') conn (runSelectReturningList (select q)))
      pure $ ctx $> r
    L (InsertSources []) -> pure $ ctx $> []
    L (InsertSources ss) -> SourceRepositoryIOC $ do
      conn <- ask
      let q =
            Pg.insertReturning
              (DB.libraryDb ^. #source)
              (insertExpressions (fmap from ss))
              ( Pg.onConflict
                  (B.conflictingFields (\t -> (t ^. #source_uri, t ^. #idx)))
                  ( Pg.onConflictUpdateInstead
                      (^. #scanned)
                  )
              )
              (Just id)
      r <- runSourceRepositoryIOC ($(runPgDebug') conn (Pg.runPgInsertReturningList q))
      pure $ ctx $> r
    L (DeleteSources []) -> pure ctx
    L (DeleteSources ks) -> SourceRepositoryIOC $ do
      conn <- ask
      let q = delete (DB.libraryDb ^. #source) (\t -> t ^. #id `in_` fmap (\(DB.SourceKey k') -> val_ k') ks)
      r <- runSourceRepositoryIOC ($(runPgDebug') conn (runDelete q))
      pure $ ctx $> r
    R other -> SourceRepositoryIOC (alg (runSourceRepositoryIOC . hdl) other ctx)

runSourceRepositoryIO :: SourceRepositoryIOC m a -> m a
runSourceRepositoryIO = runSourceRepositoryIOC
