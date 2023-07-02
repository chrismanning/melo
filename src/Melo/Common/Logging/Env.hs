module Melo.Common.Logging.Env where

import Control.Exception.Safe
import Data.IORef
import Katip
import Melo.Env
import Melo.Common.Logging
import Melo.Common.Logging.Loki
import Network.HTTP.Client qualified as Http
import System.Exit
import System.IO

withLogging :: LoggingConfig -> Http.Manager -> IO () -> IO ()
withLogging config manager m = do
  stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
  modifyRefM logEnv (registerScribe "stdout" stdoutScribe defaultScribeSettings)
  $(logDebug) $ "Logging config: " <> showt config
  mkLokiScribe config.loki manager >>= \case
    Just lokiScribe -> do
      $(logInfo) "Initialising loki scribe"
      let bufferSize = fromMaybe (1024 * 1024) (fromIntegral <$> config.loki.bufferSize)
      modifyRefM logEnv (registerScribe "loki" lokiScribe (defaultScribeSettings & scribeBufferSize .~ bufferSize))
      finally (handle logFatalError m) (readIORef logEnv >>= closeScribes)
    _ -> handle logFatalError m
    where
      logFatalError :: SomeException -> IO ()
      logFatalError e =
        die $ "Fatal error: " <> displayException e
      modifyRefM :: IORef a -> (a -> IO a) -> IO ()
      modifyRefM r m = readIORef r >>= m >>= writeIORef r
