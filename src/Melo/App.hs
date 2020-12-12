module Melo.App where

import Control.Algebra
import Control.Carrier.Error.Church
import Control.Carrier.Reader
import Control.Concurrent
import Control.Concurrent.STM
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
import Melo.Format.Error (MetadataException)
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Network.URI
import qualified System.FSNotify as FS
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
  pool <- createPool (connect connInfo) close 1 20 10

  collectionWatchState :: TVar (H.HashMap CollectionRef FS.StopListening) <- atomically $ newTVar H.empty
  forkIO $
    catchAny (initApp collectionWatchState pool) (\e -> $(logWarnIO) $ "error initialising app: " <> show e)
  $(logInfoIO) ("starting web server" :: String)
  scottyT 5000 id (api collectionWatchState pool)

initApp :: TVar (H.HashMap CollectionRef FS.StopListening) -> Pool Connection -> IO ()
initApp collectionWatchState pool =
  runReader collectionWatchState $
    runStdoutLogging $
      runFileSystemIO $
        runReader pool $
          runFileSystemWatchServiceIO $
            runTransaction $
              withTransaction $ \conn ->
                runReader conn $
                  runError (\(e :: MetadataException) -> $(logError) $ "uncaught metadata exception: " <> show e) pure $
                    runMetadataServiceIO $
                      runSavepoint $
                        runSourceRepositoryIO $
                          runFileSystemServiceIO $
                            runCollectionRepositoryIO $
                              runCollectionServiceIO
                                initCollections

initCollections ::
  ( Has FileSystemWatchService sig m,
    Has CollectionRepository sig m,
    Has CollectionService sig m,
    Has Logging sig m
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
