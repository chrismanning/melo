{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}

module Melo.Common.Logging
  ( Logging (..),
    LogMessage (..),
    log_,
    logV_,
    logDebug,
    logDebugV,
    logDebugShow,
    logInfo,
    logInfoV,
    logInfoShow,
    logWarn,
    logWarnV,
    logWarnShow,
    logError,
    logErrorV,
    logErrorShow,
    logIO,
    logVIO,
    logDebugIO,
    logDebugVIO,
    logDebugShowIO,
    logInfoIO,
    logInfoVIO,
    logInfoShowIO,
    logWarnIO,
    logWarnVIO,
    logWarnShowIO,
    logErrorIO,
    logErrorVIO,
    logErrorShowIO,
    withLogging,
  )
where

import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception hiding (Handler)
import Control.Lens ((&),(.~))
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as L
import Data.IORef
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as LT
import Katip as K
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftString, qLocation)
import Language.Haskell.TH.Syntax qualified as TH (Lift (lift))
import Melo.Common.Logging.Loki
import Melo.Env
import Network.HTTP.Client qualified as Http
import System.Exit
import System.IO
import System.IO.Unsafe
import Witch
import Prelude hiding (log)

class Monad m => Logging m where
  log :: From s LogMessage => Namespace -> Severity -> s -> m ()
  logV :: From s LogMessage => Namespace -> Severity -> s -> K.SimpleLogPayload -> m ()

class LogRet r where
  logVRet :: [Name] -> r

instance LogRet [Name] where
  logVRet = id

instance LogRet r => LogRet (Name -> r) where
  logVRet ns n = logVRet (ns <> [n])

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    Logging m
  ) =>
  Logging (t m)
  where
  log ln sev msg = lift (log ln sev msg)
  logV ln sev msg pl = lift (logV ln sev msg pl)

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
  log ns severity msg = logIOImpl' ns severity msg mempty
  logV = logIOImpl'

logImpl :: (From s LogMessage, Logging m) => String -> Int -> s -> m ()
logImpl ns severity msg = log (Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)) (toEnum severity) msg

foldArgs :: [(String, A.Value)] -> K.SimpleLogPayload
foldArgs = foldl (\payload (name, value) -> payload <> K.sl (T.pack name) value) mempty

logVImpl :: Logging m => String -> Int -> [(String, A.Value)] -> T.Text -> m ()
logVImpl ns severity payload msg = logV (Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)) (toEnum severity) msg (foldArgs payload)

-- Convert a list of Name values to a list of expressions that evaluate to their values
namesToValues :: [Name] -> [Q Exp]
namesToValues names = fmap (appE (varE 'A.toJSON) . varE . id) names

-- Generate an expression that evaluates to a list of pairs where the first element is the binding name and the second element is the value
nameValuePairs :: [Name] -> Q Exp
nameValuePairs names = do
  let values = namesToValues names
  let pairs = zipWith (\name val -> tupE [litE (stringL (nameBase name)), val]) names values
  listE pairs

logV_ :: Severity -> [Name] -> Q Exp
logV_ severity names =
  [|logVImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) $(nameValuePairs names)|]

log_ :: Severity -> Q Exp
log_ severity =
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logShow :: Severity -> Q Exp
logShow severity =
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . ((LT.pack . show) :: Show a => a -> LT.Text)|]

logDebug :: Q Exp
logDebug = log_ K.DebugS

logDebugV :: [Name] -> Q Exp
logDebugV = logV_ K.DebugS

logDebugShow :: Q Exp
logDebugShow = logShow K.DebugS

logInfo :: Q Exp
logInfo = log_ K.InfoS

logInfoV :: [Name] -> Q Exp
logInfoV = logV_ K.InfoS

logInfoShow :: Q Exp
logInfoShow = logShow K.InfoS

logWarn :: Q Exp
logWarn = log_ K.WarningS

logWarnV :: [Name] -> Q Exp
logWarnV = logV_ K.WarningS

logWarnShow :: Q Exp
logWarnShow = logShow K.WarningS

logError :: Q Exp
logError = log_ K.ErrorS

logErrorV :: [Name] -> Q Exp
logErrorV = logV_ K.ErrorS

logErrorShow :: Q Exp
logErrorShow = logShow K.ErrorS

logIOImpl :: (From s LogMessage, MonadIO m) => String -> Int -> s -> m ()
logIOImpl ns severity msg =
  let namespace = Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)
   in logIOImpl' namespace (toEnum severity) msg mempty

logVIOImpl :: (MonadIO m) => String -> Int -> [(String, A.Value)] -> T.Text -> m ()
logVIOImpl ns severity payload msg =
  let namespace = Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)
   in logIOImpl' namespace (toEnum severity) msg (foldArgs payload)

logIOImpl' :: (From s LogMessage, MonadIO m) => Namespace -> Severity -> s -> SimpleLogPayload -> m ()
logIOImpl' ns severity msg payload =
  let (LogMessage s) = from msg
   in liftIO do
    logEnv@LogEnv{..} <- readIORef logEnv
    item <- Item <$> pure _logEnvApp
                <*> pure _logEnvEnv
                <*> pure severity
                <*> (mkThreadIdText <$> myThreadId)
                <*> pure _logEnvHost
                <*> pure _logEnvPid
                <*> pure payload
                <*> pure (logStr s)
                <*> _logEnvTimer
                <*> pure (_logEnvApp <> ns)
                <*> pure Nothing
    K.runKatipT logEnv $ K.logKatipItem item

mkThreadIdText :: ThreadId -> ThreadIdText
mkThreadIdText = ThreadIdText . stripPrefix' "ThreadId " . T.pack . show
  where
    stripPrefix' pfx t = fromMaybe t (T.stripPrefix pfx t)

logIO :: Severity -> Q Exp
logIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logVIO :: Severity -> [Name] -> Q Exp
logVIO severity names =
  [|logVIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) $(nameValuePairs names)|]

logShowIO :: Severity -> Q Exp
logShowIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . ((LT.pack . show) :: Show a => a -> LT.Text)|]

logDebugIO :: Q Exp
logDebugIO = logIO K.DebugS

logDebugVIO :: [Name] -> Q Exp
logDebugVIO = logVIO K.DebugS

logDebugShowIO :: Q Exp
logDebugShowIO = logShowIO K.DebugS

logInfoIO :: Q Exp
logInfoIO = logIO K.InfoS

logInfoVIO :: [Name] -> Q Exp
logInfoVIO = logVIO K.InfoS

logInfoShowIO :: Q Exp
logInfoShowIO = logShowIO K.InfoS

logWarnIO :: Q Exp
logWarnIO = logIO K.WarningS

logWarnVIO :: [Name] -> Q Exp
logWarnVIO = logVIO K.WarningS

logWarnShowIO :: Q Exp
logWarnShowIO = logShowIO K.WarningS

logErrorIO :: Q Exp
logErrorIO = logIO K.ErrorS

logErrorVIO :: [Name] -> Q Exp
logErrorVIO = logVIO K.ErrorS

logErrorShowIO :: Q Exp
logErrorShowIO = logShowIO K.ErrorS

stdoutScribe :: Scribe
stdoutScribe = unsafePerformIO $ mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2

stdoutLogEnv :: LogEnv
stdoutLogEnv = unsafePerformIO $ registerScribe "stdout" stdoutScribe defaultScribeSettings =<< initLogEnv "melo" "local"

logEnv :: IORef LogEnv
logEnv = unsafePerformIO $ newIORef stdoutLogEnv

withLogging :: LoggingConfig -> Http.Manager -> IO () -> IO ()
withLogging config manager m = do
  hPutStrLn stderr $ show config
  mkLokiScribe config.loki manager >>= \case
    Just lokiScribe -> do
      hPutStrLn stderr "initialising loki scribe"
      let bufferSize = fromMaybe (100 * 1024) (fromIntegral <$> config.loki.bufferSize)
      let makeLogEnv = registerScribe "loki" lokiScribe (defaultScribeSettings & scribeBufferSize .~ bufferSize) stdoutLogEnv
      bracket makeLogEnv closeScribes \logEnv' -> do
        writeIORef logEnv logEnv'
        handle logFatalError m
    _ -> handle logFatalError m
    where
      logFatalError :: SomeException -> IO ()
      logFatalError e =
        die $ "Fatal error: " <> displayException e
