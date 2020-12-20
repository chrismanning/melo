{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Repo where

import Basement.From
import Control.Algebra
import Control.Effect.Exception
import Control.Effect.Reader
import Control.Effect.TH
import Control.Lens hiding (from)
import Control.Monad
import Data.Functor
import qualified Data.Text as T
import Database.Beam as B hiding (char, insert)
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import Melo.Common.NaturalSort
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Collection.Types
import Network.URI

data CollectionRepository :: Effect where
  GetAllCollections :: CollectionRepository m [DB.Collection]
  GetCollections :: [DB.CollectionKey] -> CollectionRepository m [DB.Collection]
  GetCollectionsByUri :: [URI] -> CollectionRepository m [DB.Collection]
  InsertCollections :: [NewCollection] -> CollectionRepository m [DB.Collection]
  DeleteCollections :: [DB.CollectionKey] -> CollectionRepository m ()
  DeleteAllCollections :: CollectionRepository m ()
  UpdateCollections :: [UpdateCollection] -> CollectionRepository m ()

makeSmartConstructors ''CollectionRepository

newtype CollectionRepositoryIOC m a = CollectionRepositoryIOC
  { runCollectionRepositoryIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.CollectionT)
tbl = DB.meloDb ^. #collection

instance
  (Has (Lift IO) sig m, Has (Reader Connection) sig m) =>
  Algebra (CollectionRepository :+: sig) (CollectionRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    L GetAllCollections -> ctx $$> sortNaturalBy (^. #root_uri) <$> getAll tbl
    L (GetCollections []) -> pure $ ctx $> []
    L (GetCollections ks) -> ctx $$> getByKeys tbl ks
    L (GetCollectionsByUri []) -> pure $ ctx $> []
    L (GetCollectionsByUri fs) -> CollectionRepositoryIOC $ do
      let q = filter_ (\t -> t ^. #root_uri `in_` fmap (val_ . T.pack . show) fs) (all_ tbl)
      r <- $(runPgDebug') (runSelectReturningList (select q))
      pure $ ctx $> r
    L (InsertCollections []) -> pure $ ctx $> []
    L (InsertCollections ss) -> do
      r <- do
        expr <- evaluate (insertExpressions $ fmap from ss)
        let q =
              Pg.insertReturning
                tbl
                expr
                Pg.onConflictDefault
                (Just id)
        $(runPgDebug') (Pg.runPgInsertReturningList q)
      pure $ ctx $> r
    L (DeleteCollections ks) -> ctx $$> deleteByKeys tbl ks
    L DeleteAllCollections -> ctx $$> deleteAll tbl
    L (UpdateCollections us) -> do
      forM_ us $ \u -> do
        let q = save tbl u
        $(runPgDebug') (runUpdate q)
      pure ctx
    R other -> CollectionRepositoryIOC (alg (runCollectionRepositoryIOC . hdl) other ctx)

runCollectionRepositoryIO :: CollectionRepositoryIOC m a -> m a
runCollectionRepositoryIO = runCollectionRepositoryIOC
