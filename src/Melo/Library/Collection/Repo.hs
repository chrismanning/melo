{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Repo where

import Basement.From
import Control.Concurrent.Classy
import qualified Control.Exception as E
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.Text as T
import Database.Beam as B hiding (char, insert)
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.NaturalSort
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Collection.Types
import Network.URI

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.CollectionT)
tbl = DB.meloDb ^. #collection

class Monad m => CollectionRepository m where
  getAllCollections :: m [DB.Collection]
  getCollections :: [DB.CollectionKey] -> m [DB.Collection]
  getCollectionsByUri :: [URI] -> m [DB.Collection]
  insertCollections :: [NewCollection] -> m [DB.Collection]
  deleteCollections :: [DB.CollectionKey] -> m ()
  deleteAllCollections :: m ()
  updateCollections :: [UpdateCollection] -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    CollectionRepository m
  ) =>
  CollectionRepository (t m)
  where
  getAllCollections = lift getAllCollections
  getCollections = lift . getCollections
  getCollectionsByUri = lift . getCollectionsByUri
  insertCollections = lift . insertCollections
  deleteCollections = lift . deleteCollections
  deleteAllCollections = lift deleteAllCollections
  updateCollections = lift . updateCollections

newtype CollectionRepositoryT m a = CollectionRepositoryT
  { runCollectionRepositoryT :: ReaderT Connection m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadMask, MonadThrow, MonadTrans, MonadTransControl)

instance MonadIO m => CollectionRepository (CollectionRepositoryT m) where
  getAllCollections = CollectionRepositoryT $
    ReaderT $ \conn ->
      sortNaturalBy (^. #root_uri) <$> getAllIO conn tbl
  getCollections ks = CollectionRepositoryT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  getCollectionsByUri [] = pure []
  getCollectionsByUri fs = CollectionRepositoryT $
    ReaderT $ \conn -> do
      let q = filter_ (\t -> t ^. #root_uri `in_` fmap (val_ . T.pack . show) fs) (all_ tbl)
      $(runPgDebugIO') conn (runSelectReturningList (select q)) <&> sortNaturalBy (^. #root_uri)
  insertCollections [] = pure []
  insertCollections ss = CollectionRepositoryT $
    ReaderT $ \conn -> do
      expr <- liftIO $ E.evaluate (insertExpressions $ fmap from ss)
      let q =
            Pg.insertReturning
              tbl
              expr
              Pg.onConflictDefault
              (Just id)
      $(runPgDebugIO') conn (Pg.runPgInsertReturningList q) <&> sortNaturalBy (^. #root_uri)
  deleteCollections ks = CollectionRepositoryT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  deleteAllCollections = CollectionRepositoryT $
    ReaderT $ \conn ->
      deleteAllIO conn tbl
  updateCollections us = CollectionRepositoryT $
    ReaderT $ \conn ->
      forM_ us $ \u -> do
        let q = save tbl u
        $(runPgDebugIO') conn (runUpdate q)

runCollectionRepositoryIO :: Connection -> CollectionRepositoryT m a -> m a
runCollectionRepositoryIO conn = flip runReaderT conn . runCollectionRepositoryT
