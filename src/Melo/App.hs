module Melo.App where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import qualified Data.HashMap.Strict as H
import Data.Pool
import qualified Data.Text as T
import Database.PostgreSQL.Simple
  ( ConnectInfo (..),
    Connection,
    close,
    connect,
    defaultConnectInfo,
  )
import Melo.API
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import qualified Melo.Database.Model as DB
import Melo.Database.Transaction
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Web.Scotty.Trans

app :: IO ()
app = do
  initLogging
  let connInfo =
        defaultConnectInfo
          { connectUser = "melo",
            connectPassword = "melo",
            connectDatabase = "melo"
          }
  pool <- createPool (connect connInfo) close 10 20 10

  collectionWatchState :: CollectionWatchState <- atomically $ newTVar H.empty
  fork $
    catchAny (initApp collectionWatchState pool) (\e -> $(logWarnIO) $ "error initialising app: " <> show e)
  $(logInfoIO) ("starting web server" :: String)
  scottyT 5000 id (api collectionWatchState pool)

initApp :: CollectionWatchState -> Pool Connection -> IO ()
initApp collectionWatchState pool =
  runStdoutLogging $
    runFileSystemIO $
      runTransaction pool $
        withTransaction $ \conn ->
          runSavepoint conn $
            runMetadataServiceIO $
              runSourceRepositoryIO conn $
                runFileSystemServiceIO $
                  runCollectionRepositoryIO conn $
                    runFileSystemWatchServiceIO pool collectionWatchState $
                      runCollectionServiceIO pool initCollections

initCollections ::
  ( FileSystemWatchService m,
    CollectionRepository m,
    CollectionService m,
    Logging m
  ) =>
  m ()
initCollections = do
  $(logInfo) ("initialising collections" :: String)
  collections <- getAllCollections
  forM_ collections $ \c@DB.Collection {..} -> do
    $(logDebugShow) c
    let ref = CollectionRef id
    when watch $
      case parseURI (T.unpack root_uri) >>= uriToFilePath of
        Just rootPath -> startWatching ref rootPath
        Nothing -> pure ()
    rescanCollection ref
  $(logInfo) ("collections initialised" :: String)
