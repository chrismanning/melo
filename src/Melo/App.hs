module Melo.App where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad
import Data.Default
import Data.Pool
import qualified Data.Text as T
import qualified Hasql.Connection as Hasql
import Melo.API
import Melo.Common.FileSystem
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Uri
import Melo.Database.Repo
import Melo.Library.Release.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.FileSystem.Scan
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types
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

  sess <- Wreq.newAPISession

  collectionWatchState <- emptyWatchState
  fork $
    catchAny (initApp collectionWatchState pool sess) (\e -> do
        $(logErrorIO) $ "error initialising app: " <> show e
        exitFailure
      )
  $(logInfoIO) ("starting web server" :: String)

  httpManager <- Http.newTlsManager

  let opts = def { settings = setHost "*6" $ setPort 5000 defaultSettings }
  scottyOptsT opts Prelude.id (api collectionWatchState pool sess httpManager)

initApp :: CollectionWatchState -> Pool Hasql.Connection -> Wreq.Session -> IO ()
initApp collectionWatchState pool sess =
  runStdoutLogging $
    runFileSystemIO $
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
