{-# LANGUAGE UndecidableInstances #-}
module Melo.Library.Artist.Repo.Fake where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.UUID.V4
import Melo.Common.FileSystem.Watcher
import Melo.Database.Repo
import Melo.Database.Repo.Fake
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Types
import Rel8 (Result)
import System.IO.Unsafe
import Witch

type FakeArtistRepository = FakeRepository ArtistEntity

newtype FakeArtistRepositoryT m a = FakeArtistRepositoryT {
  runFakeArtistRepositoryT :: FakeRepositoryT ArtistEntity m a
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
      MonadState FakeArtistRepository,
      Repository (ArtistTable Result)
    )

instance Monad m => ArtistRepository (FakeArtistRepositoryT m) where
  getByMusicBrainzId = undefined
  getReleaseArtists = undefined
  getSourceReleaseArtists = undefined
  getByName = undefined

instance Monad m => FileSystemWatchLocks (FakeArtistRepositoryT m) where
  lockPathsDuring _ m = m

instance From NewArtist ArtistEntity where
  from = mkNewArtist (ArtistRef $ unsafePerformIO nextRandom)

runFakeArtistRepository :: Monad m => FakeRepository (ArtistTable Result) -> FakeArtistRepositoryT m a -> m a
runFakeArtistRepository fake = runFakeRepository fake . runFakeArtistRepositoryT
