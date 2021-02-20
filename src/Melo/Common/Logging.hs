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

import Basement.From
import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Data.Time.Format
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q, liftString, qLocation)
import qualified Language.Haskell.TH.Syntax as TH (Lift (lift))
import qualified System.Wlog as Wlog
import Prelude hiding (log)

class Monad m => Logging m where
  log :: From s LogMessage => Wlog.LoggerName -> Wlog.Severity -> s -> m ()

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

--instance From BB.Builder LogMessage where
--  from = from . BL.toStrict . BB.toLazyByteString

newtype LoggingIOT m a = LoggingIOT
  { runLoggingIOT :: m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadBase b, MonadBaseControl b, MonadConc, MonadCatch, MonadMask, MonadThrow)
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance
  (MonadIO m) =>
  Logging (LoggingIOT m)
  where
  log ln severity msg = do
    let LogMessage msg' = from msg
    Wlog.logM ln severity msg'

logImpl :: (From s LogMessage, Logging m) => String -> Int -> s -> m ()
logImpl ln severity msg = log (Wlog.LoggerName $ T.pack ln) (toEnum severity) msg

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

runStdoutLogging :: LoggingIOT m a -> m a
runStdoutLogging = runLoggingIOT

logIOImpl :: (From s LogMessage, MonadIO m) => String -> Int -> s -> m ()
logIOImpl ln severity msg =
  let (LogMessage s) = from msg
   in Wlog.logM (Wlog.LoggerName $ T.pack ln) (toEnum severity) s

logIO :: Wlog.Severity -> Q Exp
logIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logShowIO :: Wlog.Severity -> Q Exp
logShowIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . ((LT.pack . show) :: Show a => a -> LT.Text)|]

logDebugIO :: Q Exp
logDebugIO = logIO Wlog.Debug

logDebugShowIO :: Q Exp
logDebugShowIO = logShowIO Wlog.Debug

logInfoIO :: Q Exp
logInfoIO = logIO Wlog.Info

logInfoShowIO :: Q Exp
logInfoShowIO = logShowIO Wlog.Info

logWarnIO :: Q Exp
logWarnIO = logIO Wlog.Warning

logWarnShowIO :: Q Exp
logWarnShowIO = logShowIO Wlog.Warning

logErrorIO :: Q Exp
logErrorIO = logIO Wlog.Error

logErrorShowIO :: Q Exp
logErrorShowIO = logShowIO Wlog.Error

initLogging :: MonadIO m => m ()
initLogging = do
  config <- Wlog.parseLoggerConfig "logging.yaml"
  Wlog.setupLogging (Just (T.pack . formatTime defaultTimeLocale "%F %T%3Q")) config
