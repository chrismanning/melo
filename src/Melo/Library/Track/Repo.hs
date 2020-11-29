{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.Repo where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens ((^.))
import Data.Containers.ListUtils (nubOrd)
import Data.Int (Int16)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import Melo.Common.Effect
import Melo.Common.Logging
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

data TrackRepository :: Effect where
  GetAllTracks :: TrackRepository m [DB.Track]
  GetTracks :: [DB.TrackKey] -> TrackRepository m [DB.Track]
  GetTrackSource :: DB.TrackKey -> TrackRepository m DB.Source
  GetTrackArtists :: DB.TrackKey -> TrackRepository m [DB.Artist]
  GetTrackAlbum :: DB.TrackKey -> TrackRepository m DB.Album
  GetTrackGenres :: DB.TrackKey -> TrackRepository m [DB.Genre]
  SearchTracks :: Text -> TrackRepository m [DB.Track]
  InsertTracks :: [NewTrack] -> TrackRepository m [DB.TrackKey]
  DeleteTracks :: [DB.TrackKey] -> TrackRepository m ()

getAllTracks :: Has TrackRepository sig m => m [DB.Track]
getAllTracks = send GetAllTracks

getTracks :: Has TrackRepository sig m => [DB.TrackKey] -> m [DB.Track]
getTracks ks = send (GetTracks ks)

getTrackSource :: Has TrackRepository sig m => DB.TrackKey -> m DB.Source
getTrackSource k = send (GetTrackSource k)

getTrackArtists :: Has TrackRepository sig m => DB.TrackKey -> m [DB.Artist]
getTrackArtists k = send (GetTrackArtists k)

getTrackAlbum :: Has TrackRepository sig m => DB.TrackKey -> m DB.Album
getTrackAlbum k = send (GetTrackAlbum k)

getTrackGenres :: Has TrackRepository sig m => DB.TrackKey -> m [DB.Genre]
getTrackGenres k = send (GetTrackGenres k)

searchTracks :: Has TrackRepository sig m => Text -> m [DB.Track]
searchTracks t = send (SearchTracks t)

insertTracks :: Has TrackRepository sig m => [NewTrack] -> m [DB.TrackKey]
insertTracks ts = send (InsertTracks ts)

deleteTracks :: Has TrackRepository sig m => [DB.TrackKey] -> m ()
deleteTracks ks = send (DeleteTracks ks)

newtype TrackRepositoryIOC m a = TrackRepositoryIOC
  { runTrackRepositoryIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.TrackT)
tbl = DB.meloDb ^. #track

instance
  ( Has (Lift IO) sig m,
    Has Logging sig m,
    Has (Reader Connection) sig m
  ) =>
  Algebra (TrackRepository :+: sig) (TrackRepositoryIOC m)
  where
  alg _hdl (L sig) ctx = case sig of
    GetAllTracks -> ctx $$> getAll tbl
    GetTracks ks -> ctx $$> getByKeys tbl ks
    DeleteTracks ks -> ctx $$> deleteByKeys tbl ks
    GetTrackSource k -> error "unimplemented"
    GetTrackArtists k -> error "unimplemented"
    GetTrackAlbum k -> error "unimplemented"
    GetTrackGenres k -> error "unimplemented"
    SearchTracks t -> error "unimplemented"
    InsertTracks ts' -> do
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
      ctx $$> $(runPgDebug') q
  alg hdl (R other) ctx = TrackRepositoryIOC (alg (runTrackRepositoryIOC . hdl) other ctx)

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

runTrackRepositoryIO :: TrackRepositoryIOC m a -> m a
runTrackRepositoryIO = runTrackRepositoryIOC
