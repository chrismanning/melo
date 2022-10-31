{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Common.Logging
  ( Logging (..),
    LogMessage (..),
    log_,
    logDebug,
    logDebugShow,
    logInfo,
    logInfoShow,
    logWarn,
    logWarnShow,
    logError,
    logErrorShow,
    runStdoutLogging,
    LoggingIOT (..),
    logIO,
    logDebugIO,
    logDebugShowIO,
    logInfoIO,
    logInfoShowIO,
    logWarnIO,
    logWarnShowIO,
    logErrorIO,
    logErrorShowIO,
    initLogging,
  )
where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens hiding (from)
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Data.Time.Format
import Katip as K
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q, liftString, qLocation)
import Language.Haskell.TH.Syntax qualified as TH (Lift (lift))
import System.IO (stdout)
import Witch hiding (over)
import Prelude hiding (log)

class Monad m => Logging m where
  log :: From s LogMessage => Namespace -> Severity -> s -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    Logging m
  ) =>
  Logging (t m)
  where
  log ln sev msg = lift (log ln sev msg)

newtype LogMessage = LogMessage {msg :: T.Text}

instance From T.Text LogMessage where
  from = LogMessage

instance From LT.Text LogMessage where
  from = from . LT.toStrict

instance From String LogMessage where
  from = from . T.pack

instance From ByteString LogMessage where
  from = from . TE.decodeUtf8

instance From L.ByteString LogMessage where
  from = from . TE.decodeUtf8 . L.toStrict

newtype LoggingIOT m a = LoggingIOT
  { runLoggingIOT :: ReaderT LogEnv m a
  }
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      PrimMonad
    )
  deriving (MonadTrans, MonadTransControl)

--  deriving (MonadReader LogEnv)

instance Logging IO where
  log ns severity msg = do
    handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
    logEnv <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "melo" "local"
    let LogMessage msg' = from msg
    K.runKatipT logEnv $ K.logMsg ns severity (logStr msg')
--    handleScribe.scribeFinalizer

instance
  (MonadIO m) =>
  Logging (LoggingIOT m)
  where
  log ns severity msg = do
    let LogMessage msg' = from msg
    K.logMsg ns severity (logStr msg')

instance MonadIO m => Katip (LoggingIOT m) where
  getLogEnv = LoggingIOT ask
  localLogEnv f (LoggingIOT m) = LoggingIOT (local f m)

-- instance MonadIO m => KatipContext (LoggingIOT m) where
--  getKatipContext = view _2
--  localKatipContext f = local (over _2 f)
--  getKatipNamespace = view _3
--  localKatipNamespace f = local (over _3 f)

logImpl :: (From s LogMessage, Logging m) => String -> Int -> s -> m ()
logImpl ns severity msg = log (Namespace [T.pack ns]) (toEnum severity) msg

log_ :: Severity -> Q Exp
log_ severity =
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logShow :: Severity -> Q Exp
logShow severity =
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . ((LT.pack . show) :: Show a => a -> LT.Text)|]

logDebug :: Q Exp
logDebug = log_ K.DebugS

logDebugShow :: Q Exp
logDebugShow = logShow K.DebugS

logInfo :: Q Exp
logInfo = log_ K.InfoS

logInfoShow :: Q Exp
logInfoShow = logShow K.InfoS

logWarn :: Q Exp
logWarn = log_ K.WarningS

logWarnShow :: Q Exp
logWarnShow = logShow K.WarningS

logError :: Q Exp
logError = log_ K.ErrorS

logErrorShow :: Q Exp
logErrorShow = logShow K.ErrorS

runStdoutLogging :: MonadIO m => LoggingIOT m a -> m a
runStdoutLogging m = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
  logEnv <- liftIO $ registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "melo" "local"
  runReaderT (runLoggingIOT m) logEnv

logIOImpl :: (From s LogMessage, MonadIO m) => String -> Int -> s -> m ()
logIOImpl ns severity msg =
  let (LogMessage s) = from msg
   in do
        handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
        logEnv <- liftIO $ registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "melo" "local"
        K.runKatipT logEnv $ K.logMsg (Namespace [T.pack ns]) (toEnum severity) (logStr s)

logIO :: Severity -> Q Exp
logIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logShowIO :: Severity -> Q Exp
logShowIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . ((LT.pack . show) :: Show a => a -> LT.Text)|]

logDebugIO :: Q Exp
logDebugIO = logIO K.DebugS

logDebugShowIO :: Q Exp
logDebugShowIO = logShowIO K.DebugS

logInfoIO :: Q Exp
logInfoIO = logIO K.InfoS

logInfoShowIO :: Q Exp
logInfoShowIO = logShowIO K.InfoS

logWarnIO :: Q Exp
logWarnIO = logIO K.WarningS

logWarnShowIO :: Q Exp
logWarnShowIO = logShowIO K.WarningS

logErrorIO :: Q Exp
logErrorIO = logIO K.ErrorS

logErrorShowIO :: Q Exp
logErrorShowIO = logShowIO K.ErrorS

initLogging :: MonadIO m => m ()
initLogging = do
  --  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.InfoS) K.V2
  --  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings =<< K.initLogEnv "Melo" "local"
  --  config <- Wlog.parseLoggerConfig "logging.yaml"
  --  Wlog.setupLogging (Just (T.pack . formatTime defaultTimeLocale "%F %T%3Q")) config
  pure ()
