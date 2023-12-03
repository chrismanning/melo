{-# LANGUAGE UndecidableInstances #-}
module Melo.Library.Track.Repo.Fake where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.Time
import Data.UUID.V4
import Melo.Common.FileSystem.Watcher
import Melo.Database.Repo
import Melo.Database.Repo.Fake
import Melo.Library.Track.Repo
import Melo.Library.Track.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Rel8 (Result)
import System.IO.Unsafe
import Witch

type FakeTrackRepository = FakeRepository (TrackTable Result)

newtype FakeTrackRepositoryT m a = FakeTrackRepositoryT {
  runFakeTrackRepositoryT :: FakeRepositoryT (TrackTable Result) m a
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
      MonadState FakeTrackRepository,
      Repository (TrackTable Result)
    )

instance Monad m => TrackRepository (FakeTrackRepositoryT m) where
  getByMusicBrainzId = undefined
  getBySrcRef = undefined

instance Monad m => FileSystemWatchLocks (FakeTrackRepositoryT m) where
  lockPathsDuring _ m = m

instance From NewTrack TrackEntity where
  from t =
    TrackTable
      { id = TrackRef $ unsafePerformIO nextRandom,
        title = t.title,
        track_number = t.trackNumber,
        comment = t.comment,
        release_id = t.releaseId,
        disc_number = t.discNumber,
        source_id = t.sourceId,
        length = calendarTimeTime t.length,
        musicbrainz_id = MB.mbid <$> t.musicBrainzId
      }

instance From Track TrackEntity where
  from t =
    TrackTable
      { id = TrackRef $ unsafePerformIO nextRandom,
        title = t.title,
        track_number = t.trackNumber,
        comment = t.comment,
        release_id = t.releaseRef,
        disc_number = t.discNumber,
        source_id = t.sourceRef,
        length = calendarTimeTime t.length,
        musicbrainz_id = MB.mbid <$> t.musicBrainzId
      }

runFakeTrackRepository :: Monad m => FakeRepository (TrackTable Result) -> FakeTrackRepositoryT m a -> m a
runFakeTrackRepository fake = runFakeRepository fake . runFakeTrackRepositoryT
