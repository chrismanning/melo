{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Containers.ListUtils (nubOrd)
import Data.Int (Int16)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import qualified Melo.Database.Model as DB
import Melo.Database.Query

data NewTrack = NewTrack
  { title :: Text,
    trackNumber :: Maybe Int16,
    discNumber :: Maybe Int16,
    comment :: Maybe Text,
    sourceId :: DB.SourceKey,
    albumId :: DB.AlbumKey,
    length :: Maybe NominalDiffTime
  }
  deriving (Generic, Eq, Ord, Show)

class Monad m => TrackRepository m where
  getAllTracks :: m [DB.Track]
  getTracks :: [DB.TrackKey] -> m [DB.Track]
  getTrackSource :: DB.TrackKey -> m DB.Source
  getTrackArtists :: DB.TrackKey -> m [DB.Artist]
  getTrackAlbum :: DB.TrackKey -> m DB.Album
  getTrackGenres :: DB.TrackKey -> m [DB.Genre]
  searchTracks :: Text -> m [DB.Track]
  insertTracks :: [NewTrack] -> m [DB.TrackKey]
  deleteTracks :: [DB.TrackKey] -> m ()

newtype TrackRepositoryIOT m a = TrackRepositoryIOT
  { runTrackRepositoryIOT :: ReaderT Connection m a
  }
  deriving newtype (Applicative, Functor, Monad)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.TrackT)
tbl = DB.meloDb ^. #track

instance
  ( MonadIO m
  ) =>
  TrackRepository (TrackRepositoryIOT m)
  where
  getAllTracks = TrackRepositoryIOT $
    ReaderT $ \conn ->
      getAllIO conn tbl
  getTracks ks = TrackRepositoryIOT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  getTrackSource k = error "unimplemented"
  getTrackArtists k = error "unimplemented"
  getTrackAlbum k = error "unimplemented"
  getTrackGenres k = error "unimplemented"
  deleteTracks ks = TrackRepositoryIOT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  searchTracks t = error "unimplemented"
  insertTracks ts' = TrackRepositoryIOT $
    ReaderT $ \conn -> do
      let !ts = nubOrd ts'
      let q =
            runPgInsertReturningList $
              insertReturning
                tbl
                ( insertExpressions
                    (fmap newTrack ts)
                )
                onConflictDefault
                (Just primaryKey)
      $(runPgDebugIO') conn q

newTrack :: NewTrack -> DB.TrackT (QExpr Postgres s)
newTrack t =
  DB.Track
    { id = default_,
      title = val_ $ t ^. #title,
      track_number = val_ $ t ^. #trackNumber,
      comment = val_ $ t ^. #comment,
      album_id = val_ $ t ^. #albumId,
      disc_number = val_ $ t ^. #discNumber,
      source_id = val_ $ t ^. #sourceId,
      length = fromMaybe_ (val_ (DB.Interval 0)) (val_ (DB.Interval <$> t ^. #length))
    }

runTrackRepositoryIO :: Connection -> TrackRepositoryIOT m a -> m a
runTrackRepositoryIO conn = flip runReaderT conn . runTrackRepositoryIOT
