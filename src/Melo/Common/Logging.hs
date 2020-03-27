{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Logging
  ( Logging (..),
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
    LoggingC (..),
    initLogging,
  )
where

import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Format
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q, liftString, qLocation)
import qualified Language.Haskell.TH.Syntax as TH (Lift (lift))
import Melo.Common.Effect
import qualified System.Wlog as Wlog

data Logging :: Effect where
  Log :: Wlog.LoggerName -> Wlog.Severity -> LT.Text -> Logging m ()

newtype LoggingC m a
  = LoggingC
      { runLoggingC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (Has (Lift IO) sig m, Algebra sig m) =>
  Algebra (Logging :+: sig) (LoggingC m)
  where
  alg _ (L (Log ln severity msg)) ctx = do
    sendM @IO $ Wlog.logM ln severity (LT.toStrict msg)
    pure ctx
  alg hdl (R other) ctx = LoggingC $ alg (runLoggingC . hdl) other ctx

logImpl :: Has Logging sig m => String -> Int -> LT.Text -> m ()
logImpl ln severity msg = send (Log (Wlog.LoggerName $ T.pack ln) (toEnum severity) msg)

log_ :: Wlog.Severity -> Q Exp
log_ severity =
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logShow :: Wlog.Severity -> Q Exp
logShow severity =
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . ((LT.pack . show) :: Show a => a -> LT.Text)|]

logDebug :: Q Exp
logDebug = log_ Wlog.Debug

logDebugShow :: Q Exp
logDebugShow = logShow Wlog.Debug

logInfo :: Q Exp
logInfo = log_ Wlog.Info

logInfoShow :: Q Exp
logInfoShow = logShow Wlog.Info

logWarn :: Q Exp
logWarn = log_ Wlog.Warning

logWarnShow :: Q Exp
logWarnShow = logShow Wlog.Warning

logError :: Q Exp
logError = log_ Wlog.Error

logErrorShow :: Q Exp
logErrorShow = logShow Wlog.Error

runStdoutLogging :: LoggingC m a -> m a
runStdoutLogging = runLoggingC

initLogging :: MonadIO m => m ()
initLogging = do
  config <- Wlog.parseLoggerConfig "logging.yaml"
  Wlog.setupLogging (Just (T.pack . formatTime defaultTimeLocale "%T%3Q")) config
