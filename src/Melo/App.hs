module Melo.App where

import Colog.Core qualified as Colog
import Configuration.Dotenv qualified as Dotenv
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception.Safe (catch)
import Control.Monad.Class.MonadThrow as E (Handler (..), Exception (..))
import Control.Monad.Class.MonadFork
import Data.Pool
import Data.Text qualified as T
import Katip (Namespace (..), Severity (..))
import Melo.API as API
import Melo.Common.API
import Melo.Common.Config
import Melo.Common.Exception (SomeException (..))
import Melo.Common.Exit
import Melo.Common.Logging as Logging
import Melo.Common.Logging.Env
import Melo.Common.Monad hiding (atomically, writeTVar)
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
import Network.RSocket as RSocket
import Network.RSocket.Transport.TCP as TCP
import Network.Socket

app :: IO ()
app = runAppM do
  Dotenv.loadFile Dotenv.defaultConfig
  env <- initEnv
  withLogging env.logging $ withGlobalTracer do
    $(logInfoIO) "Starting melo..."
    $(logDebugIO) $ "Env: " <> showt env
    putAppData (into @DB.Config env.database)

    catch
      (do
        pool <- DB.getConnectionPool
        liftIO $ withResource pool (const $ pure ())
      )
      ( \(SomeException e) -> do
          let cause = displayException e
          $(logErrorVIO ['cause]) "Failed to acquire database connection"
          exitFailure
      )

    forkIO $
      catch
        initApp
        ( \(SomeException e) -> do
            let cause = displayException e
            $(logErrorVIO ['cause]) "Failed to initialise melo"
            exitFailure
        )
    let !addr = SockAddrInet (fromIntegral env.server.port.unwrap) 0
    forever do
      RSocket.runServer addr RSocket.Config {
        logger,
        onConnection = onConnection . eraseConnection,
        customiseTransport,
        exceptionHandlers
      }
  where
    logger = Colog.LogAction (\(Colog.WithSeverity msg sev) -> Logging.log (Namespace ["Melo", "App", "RSocket"]) (mapSeverity sev) (T.pack msg))
    customiseTransport :: RSocket.Customiser TCP (AppM IO IO)
    customiseTransport sock = liftIO do
      TCP.defaultCustomiser sock
    exceptionHandlers :: [E.Handler (AppM IO IO) Bool]
    exceptionHandlers =
      [ E.Handler
          \(SomeException e) -> do
            let cause = displayException e
            $(logErrorVIO ['cause]) "Server error"
            pure False
      ]
    onConnection :: ErasedConnection (AppM IO IO) -> AppM IO IO (RSocket.SetupHandler (AppM IO IO))
    onConnection conn = do
      $(logInfoVIO ['remoteAddress]) "RSocket connection opened"
      $(logDebugVIO ['remoteAddress]) ("RSocket connection details: " <> T.pack (show conn.handle))
      pure
        SetupHandler
          { exceptionHandlers =
              [ E.Handler
                  ( \(SomeException e) -> do
                      let cause = displayException e
                      $(logErrorVIO ['cause]) "RSocket connection setup failed"
                      $(logInfoVIO ['remoteAddress]) "Stopping connection"
                      atomically $ writeTVar conn.running False
                  )
              ],
            handleLease = leaseUnsupported conn,
            handleResume = resumeUnsupported conn,
            onSetup = onSetup conn
          }
      where
        remoteAddress = T.pack $! show conn.handle.addr
        {-# NOINLINE remoteAddress #-}
        onSetup conn setup = do
          $(logDebugVIO ['remoteAddress]) ("Setup received: " <> T.pack (show setup))
          let !metadataMimeType = RawMimeType setup.metadataMimeType
          metadata <- parseMetadata logger metadataMimeType setup.metadataPayload
          let !dataMimeType = RawMimeType setup.payloadMimeType
          $(logDebugVIO ['remoteAddress]) ("Setup metadata: " <> T.pack (show metadata))
          let !exceptionHandlers = apiStreamExceptionHandlers conn
          pure
            ConnectionHandler
              { readExceptionHandlers = exceptionHandlers,
                writeExceptionHandlers = exceptionHandlers,
                handleFrame = \exceptionHandlers frame -> do
                  $(logDebugVIO ['remoteAddress]) ("Got frame: " <> T.pack (show frame))
                  _ <- handleInteractive
                    (rsocketHandlers metadataMimeType dataMimeType)
                    exceptionHandlers
                    conn
                    frame
                  pure ()
              }

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
  API.registerRoutes
  initCollections

mapSeverity :: Colog.Severity -> Severity
mapSeverity Colog.Info = InfoS
mapSeverity Colog.Warning = WarningS
mapSeverity Colog.Error = ErrorS
mapSeverity Colog.Debug = DebugS
