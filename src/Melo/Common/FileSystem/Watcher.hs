{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.FileSystem.Watcher where

import Control.Concurrent.Classy
import Control.Monad.State.Strict
import Control.Monad.Trans.Control
import Data.ByteString as BS ((!?), null)
import Data.ByteString.Char8 (ByteString (), pack)
import Data.Char
import Data.HashMap.Strict (HashMap)
import Data.Monoid
import Data.Trie (Trie ())
import Data.Trie qualified as Trie
import Data.Typeable
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Library.Collection.Types (CollectionRef)
import System.FSNotify qualified as FS

class Monad m => FileSystemWatcher m where
  startWatching :: CollectionRef -> FilePath -> m ()
  stopWatching :: CollectionRef -> m ()

instance
  {-# OVERLAPPING #-}
  ( Monad m,
    FileSystemWatcher m
  ) =>
  FileSystemWatcher (StateT s m)
  where
  startWatching ref p = lift (startWatching ref p)
  stopWatching = lift . stopWatching

class Monad m => FileSystemWatchLocks m where
  lockPathsDuring :: NonEmpty FilePath -> m a -> m a

instance
  {-# OVERLAPPING #-}
  ( Monad m,
    FileSystemWatchLocks m
  ) =>
  FileSystemWatchLocks (StateT s m)
  where
  lockPathsDuring p m = liftWith (\run -> lockPathsDuring p (run m)) >>= restoreT . pure

instance FileSystemWatchLocks (AppM IO IO) where
  lockPathsDuring ps m = do
    watchState <- getWatchState
    bracket
      ( do
          atomically $ modifyTVar' watchState.locks (lockPaths ps)
          $(logInfo) $ "Unwatching paths " <> showt (toList ps)
      )
      ( \_ -> fork do
          threadDelay 10000
          $(logInfo) $ "Re-watching paths " <> showt (toList ps)
          atomically $ modifyTVar' watchState.locks (unlockPaths ps)
      )
      (const m)

data CollectionWatchState m = CollectionWatchState
  { stoppers :: TVar (STM m) (HashMap CollectionRef FS.StopListening),
    locks :: TVar (STM m) FilePathLocks
  }
  deriving (Typeable)

emptyWatchState :: MonadConc m => m (CollectionWatchState m)
emptyWatchState =
  atomically $
    CollectionWatchState <$> newTVar mempty <*> newTVar mempty

getWatchState ::
  forall m.
  ( AppDataReader m,
    MonadConc m,
    Typeable m
  ) =>
  m (CollectionWatchState m)
getWatchState =
  getAppData @(CollectionWatchState m) >>= \case
    Just ws -> pure ws
    Nothing -> do
      ws <- emptyWatchState
      putAppData ws
      pure ws

data FilePathLocks = FilePathLocks
  { lockCounter :: Trie (Sum Int)
  }
  deriving (Show)
  deriving (TextShow) via FromStringShow FilePathLocks

instance Semigroup FilePathLocks where
  a <> b = FilePathLocks (a.lockCounter <> b.lockCounter)

instance Monoid FilePathLocks where
  mempty = FilePathLocks mempty

lockPaths :: NonEmpty FilePath -> FilePathLocks -> FilePathLocks
lockPaths newPaths locks = foldl' (\l newPath -> FilePathLocks $ Trie.alterBy lockPath (pack newPath) 1 l.lockCounter) locks newPaths
  where
    lockPath :: ByteString -> Sum Int -> Maybe (Sum Int) -> Maybe (Sum Int)
    lockPath _ c Nothing = Just c
    lockPath _ c (Just c') = Just (c <> c')

unlockPaths :: NonEmpty FilePath -> FilePathLocks -> FilePathLocks
unlockPaths oldPaths locks = foldl' (\l oldPath -> FilePathLocks $ Trie.alterBy unlockPath (pack oldPath) (-1) l.lockCounter) locks oldPaths
  where
    unlockPath :: ByteString -> Sum Int -> Maybe (Sum Int) -> Maybe (Sum Int)
    unlockPath _ _ Nothing = Nothing
    unlockPath _ _ (Just 1) = Nothing
    unlockPath _ c (Just c') = Just (c <> c')

isPathLocked :: FilePath -> FilePathLocks -> Bool
isPathLocked path locks = validMatch $ Trie.matches locks.lockCounter (pack path)
  where
    validMatch :: [(ByteString, Sum Int, ByteString)] -> Bool
    validMatch ((_, _, rem) : _) | BS.null rem = True
    validMatch ((_, _, rem) : _) | rem !? 0 == Just (fromIntegral $ ord '/') = True
    validMatch (_ : xs) = validMatch xs
    validMatch [] = False
