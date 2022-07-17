module Melo.App where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Except
import qualified Data.HashMap.Strict as H
import Data.Either
import Data.Pool
import qualified Data.Text as T
import qualified Hasql.Connection as Hasql
import Melo.API
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Metadata.Mapping.Repo
import Melo.Metadata.Mapping.Service
import System.Exit
import System.IO
import Web.Scotty.Trans

app :: IO ()
app = do
  initLogging
  $(logInfoIO) ("Starting melo..." :: String)
  let connInfo = Hasql.settings "localhost" 5432 "melo" "melo" "melo"
  let newConnection = Hasql.acquire connInfo >>= \case
                      Left e -> throwIO (ConnectionError e)
                      Right conn -> pure conn
  pool <- createPool newConnection Hasql.release 10 20 10

  catchAny (withResource pool (const $ pure ())) (\e -> do
      $(logErrorIO) $ "error acquiring database connection: " <> show e
      hPutStrLn stderr $ "error acquiring database connection: " <> show e
      exitFailure
    )

  collectionWatchState :: CollectionWatchState <- atomically $ newTVar H.empty
  fork $
    catchAny (initApp collectionWatchState pool) (\e -> do
        $(logErrorIO) $ "error initialising app: " <> show e
        exitFailure
      )
  $(logInfoIO) ("starting web server" :: String)
  scottyT 5000 Prelude.id (api collectionWatchState pool)

initApp :: CollectionWatchState -> Pool Hasql.Connection -> IO ()
initApp collectionWatchState pool =
  runStdoutLogging $
    runFileSystemIO $
      runMetadataServiceIO $
        runSourceRepositoryPooledIO pool $
          runTagMappingRepositoryPooledIO pool $
            runFileSystemServiceIO pool $
              runCollectionRepositoryPooledIO pool $
                runFileSystemWatchServiceIO pool collectionWatchState $
                  runCollectionServiceIO pool $ do
                    initCollections
                    insertDefaultMappings

initCollections ::
  ( FileSystemWatchService m,
    CollectionRepository m,
    CollectionService m,
    Logging m
  ) =>
  m ()
initCollections = do
  $(logInfo) ("initialising collections" :: String)
  collections <- getAll
  forM_ collections $ \c@CollectionTable {..} -> do
    $(logDebugShow) c
    when watch $
      case parseURI (T.unpack root_uri) >>= uriToFilePath of
        Just rootPath -> startWatching id rootPath
        Nothing -> pure ()
    rescanCollection id
  $(logInfo) ("collections initialised" :: String)
