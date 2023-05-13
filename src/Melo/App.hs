module Melo.App where

import Control.Concurrent.Classy
import Control.Monad
import Data.Default
import Data.Pool
import Data.Text qualified as T
import Hasql.Connection qualified as Hasql
import Melo.API
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Env
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.FileSystem.Scan
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types
import Melo.Library.Release.Repo
import Melo.Library.Source.Repo
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.HTTP.Client.TLS as Http
import Network.Wai.Handler.Warp
import Network.Wreq.Session qualified as Wreq
import System.Exit
import System.IO
import Web.Scotty.Trans

app :: IO ()
app = do
  env <- initEnv
  httpManager <- Http.newTlsManager
  withLogging env.logging httpManager do
    $(logInfoIO) ("Starting melo..." :: String)
    let db = env.database
    let connInfo = Hasql.settings db.host.unwrap db.port.unwrap db.user.unwrap db.password.unwrap db.database.unwrap
    let newConnection =
          Hasql.acquire connInfo >>= \case
            Left e -> throwIO (ConnectionError e)
            Right conn -> pure conn
    let poolEnv = env.database.pool
    pool <- newPool $ defaultPoolConfig newConnection Hasql.release (realToFrac poolEnv.maxIdleTime.unwrap) poolEnv.maxConnections.unwrap

    catchAny
      (withResource pool (const $ pure ()))
      ( \e -> do
          $(logErrorIO) $ "error acquiring database connection: " <> show e
          hPutStrLn stderr $ "error acquiring database connection: " <> show e
          exitFailure
      )

    sess <- Wreq.newAPISession

    collectionWatchState <- emptyWatchState
    fork $
      catchAny
        (initApp collectionWatchState pool sess)
        ( \e -> do
            $(logErrorIO) $ "error initialising app: " <> show e
            exitFailure
        )
    $(logInfoIO) ("starting web server" :: String)

    let opts = def {settings = setHost "*6" $ setPort env.server.port.unwrap defaultSettings}
    scottyOptsT opts Prelude.id (api collectionWatchState pool sess httpManager)

initApp :: CollectionWatchState -> Pool Hasql.Connection -> Wreq.Session -> IO ()
initApp collectionWatchState pool sess =
  runFileSystemIO $
    runConfigRepositoryPooledIO pool $
      runMetadataAggregateIO $
        runSourceRepositoryPooledIO pool $
          runTagMappingRepositoryPooledIO pool $
            runReleaseRepositoryPooledIO pool $
              runArtistNameRepositoryPooledIO pool $
                runArtistRepositoryPooledIO pool $
                  MB.runMusicBrainzServiceUnlimitedIO sess $
                    runCollectionRepositoryPooledIO pool $
                      runFileSystemWatcherIO pool collectionWatchState sess $
                        runCollectionAggregateIO pool sess collectionWatchState $ do
                          insertDefaultMappings
                          initMetadataConfig
                          initCollections

initCollections ::
  ( FileSystemWatcher m,
    CollectionRepository m,
    CollectionAggregate m,
    Logging m
  ) =>
  m ()
initCollections = do
  $(logInfo) ("initialising collections" :: String)
  collections <- getAll @CollectionEntity
  forM_ collections $ \c@CollectionTable {..} -> do
    $(logDebugShow) c
    when watch $
      case parseURI (T.unpack root_uri) >>= uriToFilePath of
        Just rootPath -> startWatching id rootPath
        Nothing -> pure ()
    rescanCollection id
  $(logInfo) ("collections initialised" :: String)
