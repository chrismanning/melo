{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Aggregate where

import Control.Concurrent.Classy
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Database.Repo as Repo
import Melo.Library.Collection.FileSystem.Scan
import Melo.Library.Collection.Repo as Repo
import Melo.Library.Collection.Types

class (Monad m) => CollectionAggregate m where
  addCollection :: NewCollection -> m CollectionRef
  deleteCollection :: CollectionRef -> m (Maybe CollectionRef)
  rescanCollection :: CollectionRef -> m ()
  updateCollection :: CollectionRef -> CollectionUpdates -> m ()

instance CollectionAggregate (AppM IO IO) where
  addCollection c@NewFilesystemCollection {..} = do
    $(logInfo) $ "Adding collection " <> name
    $(logDebug) $ "Adding collection " <> showt c
    Repo.insertSingle @CollectionEntity c >>= \case
      Just CollectionTable {..} -> do
        void $ fork $ scanPathIO ScanAll id (T.unpack rootPath)
        when watch $ startWatching id (T.unpack rootPath)
        pure id
      Nothing -> error "unexpected insertCollections result"
  rescanCollection ref@(CollectionRef id) = do
    getSingle @CollectionEntity ref >>= \case
      Just CollectionTable {id, root_uri} ->
        case parseURI (T.unpack root_uri) >>= uriToFilePath of
          Just rootPath -> do
            $(logInfo) $ "re-scanning collection " <> showt id <> " at " <> showt rootPath
            scanPathIO ScanNewOrModified ref rootPath
          Nothing -> $(logWarn) $ "collection " <> showt id <> " not a local file system"
      Nothing -> $(logWarn) $ "collection " <> showt id <> " not found"
    pure ()
  deleteCollection ref = do
    stopWatching ref
    firstOf traverse <$> delete @CollectionEntity (pure ref)
  updateCollection ref updates = do
    getSingle @CollectionEntity ref >>= \case
      Just old@CollectionTable {} -> do
        $(logInfo) $ "Updating collection " <> showt old.name <> " with " <> T.pack (show (Aeson.toEncoding updates))
        let !updated = entity old
        case updates.watch of
          Just False | old.watch -> stopWatching old.id
          Just True | not old.watch -> case parseURI (T.unpack old.root_uri) >>= uriToFilePath of
            Just rootPath -> startWatching old.id rootPath
            _ -> pure ()
          _ -> pure ()
        updateSingle' @CollectionEntity updated
      Nothing -> $(logWarn) $ "collection " <> showt ref <> " not found"
    pure ()
    where
      entity old =
        CollectionTable
          { id = ref,
            root_uri = old.root_uri,
            name = fromMaybe old.name updates.name,
            watch = fromMaybe old.watch updates.watch,
            kind = fromMaybe old.kind updates.kind,
            rescan = fromMaybe old.rescan updates.rescan,
            library = fromMaybe old.library updates.library
          }

getCollectionsByKey :: (CollectionRepository m) => Vector CollectionRef -> m (Vector CollectionEntity)
getCollectionsByKey = getByKey

initCollections ::
  ( FileSystemWatcher m,
    CollectionRepository m,
    CollectionAggregate m,
    Logging m
  ) =>
  m ()
initCollections = do
  $(logInfo) "initialising collections"
  collections <- getAll @CollectionEntity
  forM_ collections $ \c@CollectionTable {..} -> do
    $(logDebugShow) c
    when watch $
      case parseURI (T.unpack root_uri) >>= uriToFilePath of
        Just rootPath -> startWatching id rootPath
        Nothing -> pure ()
    when rescan do
      rescanCollection id
  $(logInfo) "collections initialised"
