module Melo.Common.Logging.Env where

import Control.Concurrent.Async qualified as Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue qualified as BQ
import Control.Exception.Safe
import Control.Lens ((&),(.~),(%~))
import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Map.Strict as M
import Data.Text
import Katip
import Katip.Core
import Melo.Env
import Melo.Common.Logging
import Melo.Common.Logging.Loki
import Network.HTTP.Client qualified as Http
import System.Exit
import System.IO

withLogging :: LoggingConfig -> Http.Manager -> IO () -> IO ()
withLogging config manager m = do
  stdoutScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
  modifyRefM logEnv (registerScribe' "stdout" stdoutScribe defaultScribeSettings)
  $(logDebug) $ "Logging config: " <> show config
  mkLokiScribe config.loki manager >>= \case
    Just lokiScribe -> do
      $(logInfo) ("Initialising loki scribe" :: Text)
      let bufferSize = fromMaybe (1024 * 1024) (fromIntegral <$> config.loki.bufferSize)
      modifyRefM logEnv (registerScribe' "loki" lokiScribe (defaultScribeSettings & scribeBufferSize .~ bufferSize))
      finally (handle logFatalError m) (readIORef logEnv >>= closeScribes)
    _ -> handle logFatalError m
    where
      logFatalError :: SomeException -> IO ()
      logFatalError e =
        die $ "Fatal error: " <> displayException e
      modifyRefM :: IORef a -> (a -> IO a) -> IO ()
      modifyRefM r m = readIORef r >>= m >>= writeIORef r

registerScribe'
    :: Text
    -> Scribe
    -> ScribeSettings
    -> LogEnv
    -> IO LogEnv
registerScribe' name scribe ScribeSettings {..} le = do
  queue <- atomically (BQ.newTBQueue (fromIntegral _scribeBufferSize))
  worker <- spawnScribeWorker' scribe queue
  let fin = do
        atomically (BQ.writeTBQueue queue PoisonPill)
        -- wait for our worker to finish final write
        void (Async.waitCatch worker)
        -- wait for scribe to finish final write
        void (scribeFinalizer scribe)

  let sh = ScribeHandle (scribe { scribeFinalizer = fin }) queue
  return (le & logEnvScribes %~ M.insert name sh)

spawnScribeWorker' :: Scribe -> BQ.TBQueue WorkerMessage -> IO (Async.Async ())
spawnScribeWorker' (Scribe write _ _) queue = Async.async go
  where
    go = do
      newCmd <- atomically (BQ.readTBQueue queue)
      case newCmd of
        NewItem a  -> do
          -- Swallow any direct exceptions from the
          -- scribe. safe-exceptions won't catch async exceptions.
          void (tryAny (write a))
          go
        PoisonPill -> return ()
