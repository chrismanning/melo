module Melo.App where

import Control.Concurrent.Classy
import Data.Default
import Data.Pool
import Melo.API
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.Exit
import Melo.Common.Logging
import Melo.Common.Logging.Env
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Metadata.Aggregate
import Melo.Database.Repo.IO as DB
import Melo.Env
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.Repo
import Melo.Library.Release.Repo
import Melo.Library.Source.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz qualified as MB
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.Wai.Handler.Warp
import OpenTelemetry.Instrumentation.Wai
import Web.Scotty.Trans

app :: IO ()
app = runAppM do
  env <- initEnv
  withLogging env.logging $ withGlobalTracer do
    $(logInfoIO) "Starting melo..."
    $(logDebugIO) $ "Env: " <> showt env
    putAppData (into @DB.Config env.database)

    catchAny
      (do
        pool <- DB.getConnectionPool
        liftIO $ withResource pool (const $ pure ())
      )
      ( \e -> do
          let cause = displayException e
          $(logErrorVIO ['cause]) "Failed to acquire database connection"
          exitFailure
      )

    fork $
      catchAny
        initApp
        ( \e -> do
            let cause = displayException e
            $(logErrorVIO ['cause]) "Failed to initialise melo"
            exitFailure
        )
    $(logInfoIO) "Starting web server"

    appData <- ask

    otel <- liftIO newOpenTelemetryWaiMiddleware
    let opts = def {settings = setHost "*6" $ setPort env.server.port.unwrap defaultSettings}
    scottyOptsT opts (runReaderT' appData) (api [otel])

initApp :: AppM IO IO ()
initApp = do
  initArtistRepo
  initArtistNameRepo
  initCollectionRepo
  initConfigRepo
  initReleaseRepo
  initSourceRepo
  initTrackRepo
  initTagMappingRepo
  initMetadataConfig
  MB.initMusicBrainzConfig
  insertDefaultMappings
  initCollections
