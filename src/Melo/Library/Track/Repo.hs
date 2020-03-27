{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Algebra
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Functor
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import Melo.Common.Effect
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

data TrackRepository :: Effect where
  GetAllTracks :: TrackRepository m [DB.Track]
  GetTracksById :: [DB.TrackKey] -> TrackRepository m [DB.Track]
  GetTrackMetadataSource :: DB.TrackKey -> TrackRepository m [DB.MetadataSource]
  GetTrackArtists :: DB.TrackKey -> TrackRepository m [DB.Artist]
  GetTrackAlbum :: DB.TrackKey -> TrackRepository m (Maybe DB.Album)
  SearchTracks :: Text -> TrackRepository m [DB.Track]
  InsertTracks :: [NewTrack] -> TrackRepository m [DB.TrackKey]
  DeleteTracks :: [DB.TrackKey] -> TrackRepository m ()

getAllTracks :: Has TrackRepository sig m => m [DB.Track]
getAllTracks = send GetAllTracks

getTracksById :: Has TrackRepository sig m => [DB.TrackKey] -> m [DB.Track]
getTracksById ks = send (GetTracksById ks)

getTrackMetadataSource :: Has TrackRepository sig m => DB.TrackKey -> m [DB.MetadataSource]
getTrackMetadataSource k = send (GetTrackMetadataSource k)

getTrackArtists :: Has TrackRepository sig m => DB.TrackKey -> m [DB.Artist]
getTrackArtists k = send (GetTrackArtists k)

getTrackAlbum :: Has TrackRepository sig m => DB.TrackKey -> m (Maybe DB.Album)
getTrackAlbum k = send (GetTrackAlbum k)

searchTracks :: Has TrackRepository sig m => Text -> m [DB.Track]
searchTracks t = send (SearchTracks t)

insertTracks :: Has TrackRepository sig m => [NewTrack] -> m [DB.TrackKey]
insertTracks ts = send (InsertTracks ts)

deleteTracks :: Has TrackRepository sig m => [DB.TrackKey] -> m ()
deleteTracks ks = send (DeleteTracks ks)

newtype TrackRepositoryIOC m a
  = TrackRepositoryIOC
      { runTrackRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m) =>
  Algebra (TrackRepository :+: sig) (TrackRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    L (InsertTracks ts) -> TrackRepositoryIOC $ do
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
      (ctx $>) <$> runTrackRepositoryIOC (runPgDebug conn q)
    R other -> TrackRepositoryIOC (alg (runTrackRepositoryIOC . hdl) (R other) ctx)

newTrack :: NewTrack -> DB.TrackT (QExpr Postgres s)
newTrack t =
  DB.Track
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
