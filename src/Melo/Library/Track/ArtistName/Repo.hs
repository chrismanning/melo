{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Track.ArtistName.Repo where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Foldl (PrimMonad)
import Control.Lens (firstOf)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Connection
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
import Witch

class Monad m => TrackArtistNameRepository m where
  getTrackArtistNames :: TrackRef -> m (Vector ArtistNameEntity)
  insert' :: Vector TrackArtistNameEntity -> m Int
  insert :: Vector TrackArtistNameEntity -> m (Vector TrackArtistNameEntity)

insertSingle :: TrackArtistNameRepository m => TrackArtistNameEntity -> m (Maybe TrackArtistNameEntity)
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

newtype TrackArtistNameRepositoryIOT m a = TrackArtistNameRepositoryIOT
  { runTrackArtistNameRepositoryIOT :: ReaderT DbConnection m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader DbConnection,
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad
    )

instance MonadIO m => TrackArtistNameRepository (TrackArtistNameRepositoryIOT m) where
  getTrackArtistNames trackRef = do
    connSrc <- ask
    runSelect connSrc do
      trackArtistName <- Rel8.each trackArtistNameSchema
      artistName <- Rel8.each artistNameSchema
      Rel8.where_ $
        trackArtistName.track_id ==. lit trackRef
          &&. artistName.id ==. trackArtistName.artist_name_id
      pure artistName
  insert' trackArtistNames | V.null trackArtistNames = pure 0
  insert' trackArtistNames = do
    connSrc <- ask
    runInsert
      connSrc
      Rel8.Insert
        { into = trackArtistNameSchema,
          rows = Rel8.values (from <$> trackArtistNames),
          onConflict = Rel8.DoNothing,
          returning = fromIntegral <$> Rel8.NumberOfRowsAffected
        }
  insert trackArtistNames | V.null trackArtistNames = pure V.empty
  insert trackArtistNames = do
    connSrc <- ask
    V.fromList
      <$> runInsert
        connSrc
        Rel8.Insert
          { into = trackArtistNameSchema,
            rows = Rel8.values (from <$> trackArtistNames),
            onConflict = Rel8.DoNothing,
            returning = Rel8.Projection (\x -> x)
          }

trackArtistNameSchema :: Rel8.TableSchema (TrackArtistNameTable Rel8.Name)
trackArtistNameSchema =
  Rel8.TableSchema
    { name = "track_artist_name",
      schema = Nothing,
      columns =
        TrackArtistNameTable
          { track_id = "track_id",
            artist_name_id = "artist_name_id"
          }
    }

runTrackArtistNameRepositoryPooledIO :: Pool Connection -> TrackArtistNameRepositoryIOT m a -> m a
runTrackArtistNameRepositoryPooledIO pool =
  flip
    runReaderT
    (Pooled pool)
    . runTrackArtistNameRepositoryIOT

runTrackArtistNameRepositoryIO :: Connection -> TrackArtistNameRepositoryIOT m a -> m a
runTrackArtistNameRepositoryIO conn =
  flip
    runReaderT
    (Single conn)
    . runTrackArtistNameRepositoryIOT
