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
import Melo.Common.Logging.Env
import Melo.Metadata.Aggregate
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Database.Repo.IO (DbConnection(..))
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
import Network.HTTP.Client as Http
import Network.HTTP.Client.TLS as Http
import Network.Wai.Handler.Warp
import System.Exit
import System.IO
import Web.Scotty.Trans

app :: IO ()
app = do
  env <- initEnv
  httpManager <- Http.newTlsManager
  withLogging env.logging httpManager do
    $(logInfoIO) "Starting melo..."
    $(logDebugIO) $ "Env: " <> showt env
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
          $(logErrorIO) $ "error acquiring database connection: " <> showt e
          hPutStrLn stderr $ "error acquiring database connection: " <> show e
          exitFailure
      )

    collectionWatchState <- emptyWatchState
    fork $
      catchAny
        (initApp collectionWatchState pool httpManager)
        ( \e -> do
            $(logErrorIO) $ "error initialising app: " <> showt e
            exitFailure
        )
    $(logInfoIO) "starting web server"

    let opts = def {settings = setHost "*6" $ setPort env.server.port.unwrap defaultSettings}
    scottyOptsT opts Prelude.id (api collectionWatchState pool httpManager)

initApp :: CollectionWatchState -> Pool Hasql.Connection -> Http.Manager -> IO ()
initApp collectionWatchState pool httpManager =
  runFileSystemIO $
    runConfigRepositoryIO (Pooled pool) $
      runMetadataAggregateIO $
        runSourceRepositoryIO (Pooled pool) $
          runTagMappingRepositoryIO (Pooled pool) $
            runReleaseRepositoryIO (Pooled pool) $
              runArtistNameRepositoryIO (Pooled pool) $
                runArtistRepositoryIO (Pooled pool) $
                  MB.runMusicBrainzServiceIO httpManager $
                    runCollectionRepositoryIO (Pooled pool) $
                      runFileSystemWatcherIO pool collectionWatchState httpManager $
                        runCollectionAggregateIO pool httpManager collectionWatchState $ do
                          insertDefaultMappings
                          initMetadataConfig
                          MB.initMusicBrainzConfig
                          initCollections

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
    rescanCollection id
  $(logInfo) "collections initialised"
