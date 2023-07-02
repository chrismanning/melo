{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}

module Melo.Common.Logging
  ( Logging (..),
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
    logEnv,
  )
where

import Control.Concurrent (myThreadId)
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Aeson as A
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Katip as K
import Katip.Core as K
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (liftString, qLocation)
import Language.Haskell.TH.Syntax qualified as TH (Lift (lift))
import System.IO
import System.IO.Unsafe
import Prelude hiding (log)

class Monad m => Logging m where
  log :: Namespace -> Severity -> Text -> m ()
  logV :: Namespace -> Severity -> Text -> K.SimpleLogPayload -> m ()

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

instance Logging IO where
  log ns severity msg = logIOImpl' ns severity msg mempty
  logV = logIOImpl'

logImpl :: Logging m => String -> Int -> Text -> m ()
logImpl ns severity msg = log (Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)) (toEnum severity) msg

foldArgs :: [(String, A.Value)] -> K.SimpleLogPayload
foldArgs = foldl (\payload (name, value) -> payload <> K.sl (T.pack name) value) mempty

logVImpl :: Logging m => String -> Int -> [(String, A.Value)] -> Text -> m ()
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
  [|logImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . showt|]

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

logIOImpl :: MonadIO m => String -> Int -> Text -> m ()
logIOImpl ns severity msg =
  let namespace = Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)
   in logIOImpl' namespace (toEnum severity) msg mempty

logVIOImpl :: (MonadIO m) => String -> Int -> [(String, A.Value)] -> Text -> m ()
logVIOImpl ns severity payload msg =
  let namespace = Namespace (filter (not . T.null) $ T.split (== '.') $ T.pack ns)
   in logIOImpl' namespace (toEnum severity) msg (foldArgs payload)

logIOImpl' :: MonadIO m => Namespace -> Severity -> Text -> SimpleLogPayload -> m ()
logIOImpl' ns severity s payload =
  liftIO do
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
    K.runKatipT logEnv $ logKatipItem' item
  where
    logKatipItem' item = do
      LogEnv{..} <- getLogEnv
      liftIO $
        forM_ (M.elems _logEnvScribes) $ \ScribeHandle {..} -> do
          whenM (scribePermitItem shScribe item) do
            atomically (tryWriteTBQueue shChan (NewItem item)) >>= \case
              False -> do
                hPutStrLn stderr "Failed to add log item to queue"
              _ -> pure ()

logIO :: Severity -> Q Exp
logIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity)|]

logVIO :: Severity -> [Name] -> Q Exp
logVIO severity names =
  [|logVIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) $(nameValuePairs names)|]

logShowIO :: Severity -> Q Exp
logShowIO severity =
  [|logIOImpl $(qLocation >>= liftString . loc_module) $(TH.lift $ fromEnum severity) . showt|]

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

logEnv :: IORef LogEnv
logEnv = unsafePerformIO $ newIORef =<< initLogEnv "melo" "local"
