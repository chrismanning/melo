{-# LANGUAGE UndecidableInstances #-}

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
    LoggingC (..),
    initLogging,
  )
where

import Basement.From
import Control.Algebra
import Control.Carrier.Lift
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Time.Format
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q, liftString, qLocation)
import qualified Language.Haskell.TH.Syntax as TH (Lift (lift))
import Melo.Common.Effect
import qualified System.Wlog as Wlog

data Logging :: Effect where
  Log :: From s LogMessage => Wlog.LoggerName -> Wlog.Severity -> s -> Logging m ()

newtype LogMessage = LogMessage { msg :: T.Text }

instance From T.Text LogMessage where
  from = LogMessage

instance From LT.Text LogMessage where
  from = from . LT.toStrict

instance From String LogMessage where
  from = from . T.pack

instance From C8.ByteString LogMessage where
  from = from . TE.decodeUtf8

instance From BB.Builder LogMessage where
  from = from . BL.toStrict. BB.toLazyByteString

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
    let LogMessage msg' = from msg
    sendIO $ Wlog.logM ln severity msg'
    pure ctx
  alg hdl (R other) ctx = LoggingC $ alg (runLoggingC . hdl) other ctx

logImpl :: (From s LogMessage, Has Logging sig m) => String -> Int -> s -> m ()
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
