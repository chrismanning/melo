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

import Control.Concurrent (ThreadId, myThreadId, forkIO)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Katip as K
import Language.Haskell.TH.Syntax (Exp, Loc (..), Q, liftString, qLocation)
import Language.Haskell.TH.Syntax qualified as TH (Lift (lift))
import System.IO (stdout)
import System.IO.Unsafe
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

instance Logging IO where
  log ns severity msg = logIOImpl' ns severity msg

logImpl :: (From s LogMessage, Logging m) => String -> Int -> s -> m ()
logImpl ns severity msg = log (Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)) (toEnum severity) msg

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

logIOImpl :: (From s LogMessage, MonadIO m) => String -> Int -> s -> m ()
logIOImpl ns severity msg =
  let namespace = Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)
   in logIOImpl' namespace (toEnum severity) msg

logIOImpl' :: (From s LogMessage, MonadIO m) => Namespace -> Severity -> s -> m ()
logIOImpl' ns severity msg =
  let (LogMessage s) = from msg
   in liftIO do
    let LogEnv{..} = logEnv
    item <- Item <$> pure _logEnvApp
                <*> pure _logEnvEnv
                <*> pure severity
                <*> (mkThreadIdText <$> myThreadId)
                <*> pure _logEnvHost
                <*> pure _logEnvPid
                <*> pure ()
                <*> pure (logStr s)
                <*> _logEnvTimer
                <*> pure (_logEnvApp <> ns)
                <*> pure Nothing
    atomically $ writeTQueue messageQueue (LogItemWrapper item)

mkThreadIdText :: ThreadId -> ThreadIdText
mkThreadIdText = ThreadIdText . stripPrefix' "ThreadId " . T.pack . show
  where
    stripPrefix' pfx t = fromMaybe t (T.stripPrefix pfx t)

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

scribe :: Scribe
scribe = unsafePerformIO $ mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2

logEnv :: LogEnv
logEnv = unsafePerformIO $ registerScribe "stdout" scribe defaultScribeSettings =<< initLogEnv "melo" "local"

data LogItemWrapper = forall a. K.LogItem a => LogItemWrapper (K.Item a)

messageQueue :: TQueue LogItemWrapper
messageQueue = unsafePerformIO $ newTQueueIO

initLogging :: MonadIO m => m ()
initLogging = liftIO $ do
  forkIO $ forever do
    LogItemWrapper item <- atomically $ readTQueue messageQueue
    K.runKatipT logEnv $ K.logKatipItem item
  pure ()
