{-# LANGUAGE UndecidableInstances #-}
module Melo.Library.Artist.Name.Repo.Fake where

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
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Name.Types
import Rel8 (Result)
import System.IO.Unsafe
import Witch

type FakeArtistNameRepository = FakeRepository (ArtistNameTable Result)

newtype FakeArtistNameRepositoryT m a = FakeArtistNameRepositoryT {
  runFakeArtistNameRepositoryT :: FakeRepositoryT (ArtistNameTable Result) m a
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
      MonadState FakeArtistNameRepository,
      Repository (ArtistNameTable Result)
    )

instance Monad m => ArtistNameRepository (FakeArtistNameRepositoryT m) where
  getArtistNames = undefined
  getAlias = undefined

instance Monad m => FileSystemWatchLocks (FakeArtistNameRepositoryT m) where
  lockPathsDuring _ m = m

instance From NewArtistName ArtistNameEntity where
  from n =
    ArtistNameTable
      { id = ArtistNameRef $ unsafePerformIO nextRandom,
        artist_id = n.artist,
        name = n.name
      }

runFakeArtistNameRepository :: Monad m => FakeRepository (ArtistNameTable Result) -> FakeArtistNameRepositoryT m a -> m a
runFakeArtistNameRepository fake = runFakeRepository fake . runFakeArtistNameRepositoryT
