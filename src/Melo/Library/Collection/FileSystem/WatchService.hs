{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.FileSystem.WatchService where

import Control.Algebra
import Control.Carrier.Error.Church
import Control.Carrier.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Effect.Exception
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.State
import Control.Effect.TH
import Control.Lens
import Control.Monad
import Data.Functor
import Data.HashMap.Strict as H
import Data.Maybe
import Data.Pool
import Data.Time (NominalDiffTime)
import Database.PostgreSQL.Simple (Connection)
import Melo.Common.Effect
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Database.Transaction
import qualified Melo.Format as F
import qualified Melo.Format.Error as F
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types
import System.FSNotify (Debounce (..), WatchConfig (..))
import qualified System.FSNotify as FS
import System.FilePath

data FileSystemWatchService :: Effect where
  StartWatching :: CollectionRef -> FilePath -> FileSystemWatchService m ()
  StopWatching :: CollectionRef -> FileSystemWatchService m ()

makeSmartConstructors ''FileSystemWatchService

newtype FileSystemWatchServiceIOC m a = FileSystemWatchServiceIOC
  { runFileSystemWatchServiceIOC :: m a
  }
  deriving (Functor, Applicative, Monad)

runFileSystemWatchServiceIO :: FileSystemWatchServiceIOC m a -> m a
runFileSystemWatchServiceIO = runFileSystemWatchServiceIOC

instance
  ( Has (Lift IO) sig m,
    Has (Reader (Pool Connection)) sig m,
    Has FileSystem sig m,
    Has (Reader (TVar (H.HashMap CollectionRef FS.StopListening))) sig m,
    Has Logging sig m
  ) =>
  Algebra (FileSystemWatchService :+: sig) (FileSystemWatchServiceIOC m)
  where
  alg hdl sig ctx = case sig of
    L (StartWatching ref p) -> do
      $(logInfo) $ "starting to watch path " <> p
      pool <- ask
      stoppers <- ask @(TVar (H.HashMap CollectionRef FS.StopListening))
      sendIO $
        forkIO $
          FS.withManagerConf (FS.defaultConfig {confDebounce = Debounce (0.100 :: NominalDiffTime)}) $ \watchManager -> do
            stop <- FS.watchTree watchManager p (const True) (handleEventIO pool ref)
            atomically $ modifyTVar' stoppers (H.insert ref stop)
            forever $ threadDelay 1000000
      pure ctx
    L (StopWatching ref) -> do
      stoppers <- ask @(TVar (H.HashMap CollectionRef FS.StopListening))
      stoppers' <- sendIO $ atomically $ readTVar stoppers
      ctx $$> case H.lookup ref stoppers' of
        Just stop -> sendIO stop
        Nothing -> pure ()
    R other -> FileSystemWatchServiceIOC (alg (runFileSystemWatchServiceIOC . hdl) other ctx)

handleEventIO :: Pool Connection -> CollectionRef -> FS.Event -> IO ()
handleEventIO pool ref event = do
  runStdoutLogging $ $(logDebug) $ "handling fs event " <> show event
  _threadId <- forkIO $
    runStdoutLogging $
      runFileSystemIO $
        runReader pool $
          runTransaction $
            withTransaction $ \conn ->
              runReader conn $
                runError
                  (\(e :: F.MetadataException) -> $(logError) $ "error handling file system event: " <> show e)
                  (const $ pure ())
                  $ runMetadataServiceIO $
                    runSavepoint $
                      runSourceRepositoryIO $
                        runFileSystemServiceIO $
                          handleEvent ref event
  pure ()

handleEvent ::
  ( Has Logging sig m,
    Has FileSystemService sig m
  ) =>
  CollectionRef ->
  FS.Event ->
  m ()
handleEvent ref event =
  case event of
    FS.Added p _ _ -> do
      $(logInfo) $ "file/directory added; scanning " <> p
      scanPath ref p
      pure ()
    FS.Modified p _ _ -> do
      $(logWarn) $ "file/directory modified; scanning " <> p
      scanPath ref p
      pure ()
    FS.Removed p t _ -> do
      $(logWarn) $ "file removed " <> p

    --      deleteSources []
    FS.Unknown p _ s ->
      $(logWarn) $ "unknown file system event on path " <> p <> ": " <> s
