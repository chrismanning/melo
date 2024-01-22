{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.ArtistName.Repo where

import Data.Int
import Data.Vector qualified as V
import Melo.Common.Monad
import Melo.Database.Repo.IO
import Melo.Library.Artist.Name.Repo (artistNameSchema)
import Melo.Library.Artist.Name.Types
  ( ArtistNameEntity,
    ArtistNameTable (..),
  )
import Melo.Library.Track.ArtistName.Types
import Melo.Library.Track.Types
import Rel8 (lit, (&&.), (==.))
import Rel8 qualified
import Streaming qualified as S

class (Monad m) => TrackArtistNameRepository m where
  getTrackArtistNames :: TrackRef -> m (Vector ArtistNameEntity)
  insert' :: Vector TrackArtistNameEntity -> m Int64
  insert :: Vector TrackArtistNameEntity -> m (Vector TrackArtistNameEntity)

insertSingle :: (TrackArtistNameRepository m) => TrackArtistNameEntity -> m (Maybe TrackArtistNameEntity)
insertSingle e = firstOf traverse <$> insert (V.singleton e)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    TrackArtistNameRepository m
  ) =>
  TrackArtistNameRepository (t m)
  where
  getTrackArtistNames = lift . getTrackArtistNames
  insert' = lift . insert'
  insert = lift . insert

instance TrackArtistNameRepository (AppM IO IO) where
  getTrackArtistNames trackRef = do
    pool <- getConnectionPool
    runSelect pool do
      trackArtistName <- Rel8.each trackArtistNameSchema
      artistName <- Rel8.each artistNameSchema
      Rel8.where_ $
        trackArtistName.track_id ==. lit trackRef
          &&. artistName.id ==. trackArtistName.artist_name_id
      pure artistName
  insert' trackArtistNames | V.null trackArtistNames = pure 0
  insert' trackArtistNames = do
    pool <- getConnectionPool
    runInsert
      pool
      Rel8.runN
      Rel8.Insert
        { into = trackArtistNameSchema,
          rows = Rel8.values (from <$> trackArtistNames),
          onConflict = Rel8.DoNothing,
          returning = Rel8.NoReturning
        }
  insert trackArtistNames | V.null trackArtistNames = pure V.empty
  insert trackArtistNames = do
    pool <- getConnectionPool
    runInsert
      pool
      Rel8.runVector
      Rel8.Insert
        { into = trackArtistNameSchema,
          rows = Rel8.values (from <$> trackArtistNames),
          onConflict = Rel8.DoNothing,
          returning = Rel8.Returning (\x -> x)
        }

instance
  {-# OVERLAPPING #-}
  ( TrackArtistNameRepository m,
    Functor f
  ) =>
  TrackArtistNameRepository (S.Stream f m)
  where
  getTrackArtistNames = lift . getTrackArtistNames
  insert' = lift . insert'
  insert = lift . insert

trackArtistNameSchema :: Rel8.TableSchema (TrackArtistNameTable Rel8.Name)
trackArtistNameSchema =
  Rel8.TableSchema
    { name = "track_artist_name",
      columns =
        TrackArtistNameTable
          { track_id = "track_id",
            artist_name_id = "artist_name_id"
          }
    }
