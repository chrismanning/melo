module Melo.Common.Logging.Env where

import Control.Exception.Safe
import Katip
import Melo.Common.Exit
import Melo.Common.Logging
import Melo.Common.Logging.Loki
import Melo.Common.Monad
import Melo.Env
import System.Console.ANSI
import System.IO

withLogging ::
  forall m.
  ( AppDataReader m,
    MonadMask m,
    MonadIO m
  ) =>
  LoggingConfig ->
  m () ->
  m ()
withLogging config m = do
  liftIO do
    withColour <- hSupportsANSIColor stdout
    stdoutScribe <- mkHandleScribe (ColorLog withColour) stdout permitItem' V2
    modifyRefM logEnv (registerScribe "stdout" stdoutScribe defaultScribeSettings)
  $(logDebugIO) $ "Logging config: " <> showt config
  mkLokiScribe config.loki >>= \case
    Just lokiScribe -> do
      $(logInfoIO) "Initialising loki scribe"
      let bufferSize = fromMaybe (1024 * 1024) (fromIntegral <$> config.loki.bufferSize)
      liftIO $ modifyRefM logEnv (registerScribe "loki" lokiScribe (defaultScribeSettings & scribeBufferSize .~ bufferSize))
      finally (handle logFatalError m) closeScribes
    _ -> finally (handle logFatalError m) closeScribes
  where
    logFatalError :: SomeException -> m ()
    logFatalError e = do
      let !cause = displayException e
      $(logErrorVIO ['cause]) "Fatal error"
      die $ "Fatal error: " <> cause
    modifyRefM :: IORef IO a -> (a -> IO a) -> IO ()
    modifyRefM r m = readIORef r >>= m >>= writeIORef r
    closeScribes = liftIO (readIORef logEnv >>= Katip.closeScribes)
    permitItem' :: Item a -> IO Bool
    permitItem' = case textToSeverity (from config.console.level.unwrap) of
      Just severity -> permitItem severity
      Nothing -> const (pure False)
