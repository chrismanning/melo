{-# LANGUAGE UndecidableInstances #-}
module Melo.Library.Release.ArtistName.Repo.Fake where

import Control.Concurrent.Classy
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Maybe
import Data.Vector (Vector)
import Melo.Common.Exception
import Melo.Common.FileSystem.Watcher
import Melo.Library.Artist.Name.Types
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.ArtistName.Types
import Melo.Library.Release.Types

newtype FakeReleaseArtistNameRepository = FakeReleaseArtistNameRepository (HashMap ReleaseRef (Vector ArtistNameEntity))

newtype FakeReleaseArtistNameRepositoryT m a = FakeReleaseArtistNameRespositoryT {
  runFakeReleaseArtistNameRepositoryT :: StateT FakeReleaseArtistNameRepository m a
} deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      MonadState FakeReleaseArtistNameRepository
    )

instance Monad m => ReleaseArtistNameRepository (FakeReleaseArtistNameRepositoryT m) where
  getReleaseArtistNames ref = pure mempty --fromMaybe mempty <$> gets (HashMap.lookup ref . coerce)
  insert' = undefined
  insert = undefined

instance Monad m => FileSystemWatchLocks (FakeReleaseArtistNameRepositoryT m) where
  lockPathsDuring _ m = m

runFakeReleaseArtistNameRepository :: Monad m => FakeReleaseArtistNameRepository -> FakeReleaseArtistNameRepositoryT m a -> m a
runFakeReleaseArtistNameRepository fake = flip evalStateT fake . runFakeReleaseArtistNameRepositoryT
