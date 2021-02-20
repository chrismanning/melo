{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Repo where

import Basement.From
import Control.Concurrent.Classy
import Control.Exception (evaluate)
import Control.Exception.Safe
import Control.Lens hiding (from)
import Control.Monad
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.Text as T
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.NaturalSort
import qualified Melo.Database.Model as DB
import Melo.Database.Query
import Melo.Library.Source.Types
import Network.URI

class Monad m => SourceRepository m where
  getAllSources :: m [DB.Source]
  getSources :: [DB.SourceKey] -> m [DB.Source]
  getSourcesByUri :: [URI] -> m [DB.Source]
  getSourceKeysByUri :: [URI] -> m [DB.SourceKey]
  getSourcesByUriPrefix :: URI -> m [DB.Source]
  getSourceKeysByUriPrefix :: URI -> m [DB.SourceKey]
  insertSources :: [NewSource] -> m [DB.Source]
  deleteSources :: [DB.SourceKey] -> m ()
  updateSources :: [UpdateSource] -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    SourceRepository m
  ) =>
  SourceRepository (t m)
  where
  getAllSources = lift getAllSources
  getSources = lift . getSources
  getSourcesByUri = lift . getSourcesByUri
  getSourceKeysByUri = lift . getSourceKeysByUri
  getSourcesByUriPrefix = lift . getSourcesByUriPrefix
  getSourceKeysByUriPrefix = lift . getSourceKeysByUriPrefix
  insertSources = lift . insertSources
  deleteSources = lift . deleteSources
  updateSources = lift . updateSources

newtype SourceRepositoryIOT m a = SourceRepositoryIOT
  { runSourceRepositoryIOT :: ReaderT Pg.Connection m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl
    )

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.SourceT)
tbl = DB.meloDb ^. #source

instance
  (MonadIO m) =>
  SourceRepository (SourceRepositoryIOT m)
  where
  getAllSources = SourceRepositoryIOT $
    ReaderT $ \conn ->
      sortNaturalBy (^. #source_uri) <$> getAllIO conn tbl
  getSources [] = pure []
  getSources ks = SourceRepositoryIOT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  getSourcesByUri [] = pure []
  getSourcesByUri fs = SourceRepositoryIOT $
    ReaderT $ \conn -> do
      let q = filter_ (\t -> t ^. #source_uri `in_` fmap (val_ . T.pack . show) fs) (all_ $ DB.meloDb ^. #source)
      $(runPgDebugIO') conn (runSelectReturningList (select q))
  getSourceKeysByUri [] = pure []
  getSourceKeysByUri fs = SourceRepositoryIOT $
    ReaderT $ \conn -> do
      let q = primaryKey <$> filter_ (\t -> t ^. #source_uri `in_` fmap (val_ . T.pack . show) fs) (all_ $ DB.meloDb ^. #source)
      $(runPgDebugIO') conn (runSelectReturningList (select q))
  getSourcesByUriPrefix uri = SourceRepositoryIOT $
    ReaderT $ \conn -> do
      let prefix = T.pack $ show uri
      let q = filter_ (\t -> (t ^. #source_uri) `startsWith_` val_ prefix) (all_ $ DB.meloDb ^. #source)
      $(runPgDebugIO') conn (runSelectReturningList (select q))
  getSourceKeysByUriPrefix uri = SourceRepositoryIOT $
    ReaderT $ \conn -> do
      let prefix = T.pack $ show uri
      let q = primaryKey <$> filter_ (\t -> (t ^. #source_uri) `startsWith_` val_ prefix) (all_ $ DB.meloDb ^. #source)
      $(runPgDebugIO') conn (runSelectReturningList (select q))
  insertSources [] = pure []
  insertSources ss = SourceRepositoryIOT $
    ReaderT $ \conn -> do
      expr <- liftIO $ evaluate (insertExpressions $ fmap from ss)
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
      $(runPgDebugIO') conn (Pg.runPgInsertReturningList q)
  deleteSources ks = SourceRepositoryIOT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  updateSources us = SourceRepositoryIOT $
    ReaderT $ \conn ->
      forM_ us $ \u -> do
        let q = save tbl u
        $(runPgDebugIO') conn (runUpdate q)

runSourceRepositoryIO :: Connection -> SourceRepositoryIOT m a -> m a
runSourceRepositoryIO conn = flip runReaderT conn . runSourceRepositoryIOT
