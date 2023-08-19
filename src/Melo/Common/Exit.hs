module Melo.Common.Exit (
    Exit.ExitCode(..)
  , exitWith
  , exitFailure
  , exitSuccess
  , die
  ) where

import Control.Monad.IO.Class
import GHC.IO.Exception
import Melo.Common.Exception
import System.Exit qualified as Exit
import System.IO

exitWith :: (MonadIO m, MonadThrow m) => Exit.ExitCode -> m a
exitWith Exit.ExitSuccess = throwM Exit.ExitSuccess
exitWith code@(Exit.ExitFailure n)
  | n /= 0 = throwM code
  | otherwise = liftIO $ ioError (IOError Nothing InvalidArgument "exitWith" "ExitFailure 0" Nothing Nothing)

exitFailure :: (MonadIO m, MonadThrow m) => m a
exitFailure = exitWith (Exit.ExitFailure 1)

exitSuccess :: (MonadIO m, MonadThrow m) => m a
exitSuccess = exitWith Exit.ExitSuccess

die :: (MonadIO m, MonadThrow m) => String -> m a
die err = liftIO (hPutStrLn stderr err) >> exitFailure
