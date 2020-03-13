{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Algebra
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import GHC.Generics (Generic1)
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewTrack
  = NewTrack
      { title :: Text,
        trackNumber :: Maybe Int,
        discNumber :: Maybe Int,
        comment :: Maybe Text,
        audioSourceId :: Maybe DB.AudioSourceKey,
        metadataSourceId :: DB.MetadataSourceKey,
        length :: Maybe NominalDiffTime
      }
  deriving (Generic, Eq, Show)

data TrackRepository m k
  = GetAllTracks ([DB.Track] -> m k)
  | GetTracksById [DB.TrackKey] ([DB.Track] -> m k)
  | GetTrackMetadataSource DB.TrackKey ([DB.MetadataSource] -> m k)
  | GetTrackArtists DB.TrackKey ([DB.Artist] -> m k)
  | GetTrackAlbum DB.TrackKey (Maybe DB.Album -> m k)
  | SearchTracks Text ([DB.Track] -> m k)
  | InsertTracks [NewTrack] ([DB.TrackKey] -> m k)
  | DeleteTracks [DB.TrackKey] (m k)
  deriving (Functor, Generic1, HFunctor, Effect)

getAllTracks :: Has TrackRepository sig m => m [DB.Track]
getAllTracks = send (GetAllTracks pure)

getTracksById :: Has TrackRepository sig m => [DB.TrackKey] -> m [DB.Track]
getTracksById ks = send (GetTracksById ks pure)

getTrackMetadataSource :: Has TrackRepository sig m => DB.TrackKey -> m [DB.MetadataSource]
getTrackMetadataSource k = send (GetTrackMetadataSource k pure)

getTrackArtists :: Has TrackRepository sig m => DB.TrackKey -> m [DB.Artist]
getTrackArtists k = send (GetTrackArtists k pure)

getTrackAlbum :: Has TrackRepository sig m => DB.TrackKey -> m (Maybe DB.Album)
getTrackAlbum k = send (GetTrackAlbum k pure)

searchTracks :: Has TrackRepository sig m => Text -> m [DB.Track]
searchTracks t = send (SearchTracks t pure)

insertTracks :: Has TrackRepository sig m => [NewTrack] -> m [DB.TrackKey]
insertTracks ts = send (InsertTracks ts pure)

deleteTracks :: Has TrackRepository sig m => [DB.TrackKey] -> m ()
deleteTracks ks = send (DeleteTracks ks (pure ()))

newtype TrackRepositoryIOC m a
  = TrackRepositoryIOC
      { runTrackRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig) =>
  Algebra (TrackRepository :+: sig) (TrackRepositoryIOC m)
  where
  alg = \case
    L (InsertTracks ts k) -> TrackRepositoryIOC $ do
      conn <- ask
      let q =
            runPgInsertReturningList $
              insertReturning
                (DB.libraryDb ^. #track)
                ( insertExpressions
                    (fmap newTrack ts)
                )
                onConflictDefault
                (Just primaryKey)
      runTrackRepositoryIOC $ runPgDebug conn q >>= k
    R other -> TrackRepositoryIOC (alg (R (handleCoercible other)))

newTrack :: NewTrack -> DB.TrackT (QExpr Postgres s)
newTrack t = DB.Track
  { id = default_,
    title = val_ $ t ^. #title,
    track_number = val_ $ t ^. #trackNumber,
    comment = val_ $ t ^. #comment,
    album_id = nothing_,
    disc_number = val_ $ t ^. #discNumber,
    audio_source_id = nothing_,
    metadata_source_id = val_ $ t ^. #metadataSourceId,
    length = fromMaybe_ (val_ (DB.Interval 0)) (val_ (DB.Interval <$> t ^. #length))
  }

runTrackRepositoryIO :: Connection -> TrackRepositoryIOC m a -> m a
runTrackRepositoryIO conn = runReader conn . runTrackRepositoryIOC
