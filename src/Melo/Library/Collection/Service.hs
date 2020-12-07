{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.Service where

import Control.Algebra
import Control.Effect.TH
import Control.Monad
import Data.Functor
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Common.Uri
import qualified Melo.Database.Model as DB
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo as Repo
import Melo.Library.Collection.Types
import Network.URI

data CollectionService :: Effect where
  AddCollection :: NewCollection -> CollectionService m CollectionRef
  DeleteCollection :: CollectionRef -> CollectionService m ()
  RescanCollection :: CollectionRef -> CollectionService m ()

makeSmartConstructors ''CollectionService

newtype CollectionServiceIOC m a = CollectionServiceIOC
  { runCollectionServiceIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

runCollectionServiceIO :: CollectionServiceIOC m a -> m a
runCollectionServiceIO = runCollectionServiceIOC

instance
  ( Has CollectionRepository sig m,
    Has FileSystemService sig m,
    Has FileSystemWatchService sig m,
    Has Logging sig m
  ) =>
  Algebra (CollectionService :+: sig) (CollectionServiceIOC m)
  where
  alg hdl sig ctx = case sig of
    L (AddCollection c@NewFilesystemCollection {..}) -> do
      $(logInfo) $ "Adding collection " <> name
      $(logDebug) $ "Adding collection " <> show c
      cs <- Repo.insertCollections [c]
      case cs of
        [DB.Collection {..}] -> do
          let ref = CollectionRef id
          _ <- scanPath ref (T.unpack rootPath)
          when watch $ startWatching ref (T.unpack rootPath)
          pure $ ctx $> ref
        _otherwise -> error "unexpected insertCollections result"
    L (RescanCollection ref@(CollectionRef id)) -> do
      listToMaybe <$> getCollections [DB.CollectionKey id] >>= \case
        Just DB.Collection {id, root_uri} ->
          case parseURI (T.unpack root_uri) >>= uriToFilePath of
            Just rootPath -> do
              $(logInfo) $ "re-scanning collection " <> show id <> " at " <> rootPath
              scanPath ref rootPath >> pure ()
            Nothing -> $(logWarn) $ "collection " <> show id <> " not a local file system"
        Nothing -> $(logWarn) $ "collection " <> show id <> " not found"
      pure $ ctx $> ()
    L (DeleteCollection ref@(CollectionRef id)) -> do
      stopWatching ref
      deleteCollections [DB.CollectionKey id]
      pure $ ctx $> ()
    R other -> CollectionServiceIOC (alg (runCollectionServiceIOC . hdl) other ctx)
