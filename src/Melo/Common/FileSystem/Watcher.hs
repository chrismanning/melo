{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.FileSystem.Watcher where

import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.List.NonEmpty
import Melo.Library.Collection.Types (CollectionRef)

class Monad m => FileSystemWatcher m where
  startWatching :: CollectionRef -> FilePath -> m ()
  stopWatching :: CollectionRef -> m ()
  lockPathsDuring :: NonEmpty FilePath -> m a -> m a

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MonadTransControl t,
    FileSystemWatcher m
  ) =>
  FileSystemWatcher (t m)
  where
  startWatching ref p = lift (startWatching ref p)
  stopWatching = lift . stopWatching
  lockPathsDuring p m = liftWith (\run -> lockPathsDuring p (run m)) >>= restoreT . pure
