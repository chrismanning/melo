{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Release.ArtistName.Repo where

import Control.Monad.State.Strict
import Data.Vector qualified as V
import Melo.Common.Monad
import Melo.Database.Repo.IO
import Melo.Library.Release.ArtistName.Types
import Melo.Library.Release.Types (ReleaseRef (..))
import Melo.Library.Artist.Name.Repo (artistNameSchema)
import Melo.Library.Artist.Name.Types
  ( ArtistNameEntity,
    ArtistNameTable (..),
  )
import Rel8 (Expr, Query, lit, (==.))
import Rel8 qualified

class Monad m => ReleaseArtistNameRepository m where
  getReleaseArtistNames :: ReleaseRef -> m (Vector ArtistNameEntity)
  insert' :: Vector ReleaseArtistNameEntity -> m Int
  insert :: Vector ReleaseArtistNameEntity -> m (Vector ReleaseArtistNameEntity)

insertSingle :: ReleaseArtistNameRepository m => ReleaseArtistNameEntity -> m (Maybe ReleaseArtistNameEntity)
insertSingle e = firstOf traverse <$> insert (V.singleton e)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ReleaseArtistNameRepository m
  ) =>
  ReleaseArtistNameRepository (t m)
  where
  getReleaseArtistNames = lift . getReleaseArtistNames
  insert' = lift . insert'
  insert = lift . insert

instance ReleaseArtistNameRepository (AppM IO IO) where
  getReleaseArtistNames releaseRef = do
    pool <- getConnectionPool
    runSelect pool (artistNameForReleaseRef $ lit releaseRef)
  insert' releaseArtistNames | V.null releaseArtistNames = pure 0
  insert' releaseArtistNames = do
    pool <- getConnectionPool
    runInsert
      pool
      Rel8.Insert
        { into = releaseArtistNameSchema,
          rows = Rel8.values (from <$> releaseArtistNames),
          onConflict = Rel8.DoNothing,
          returning = fromIntegral <$> Rel8.NumberOfRowsAffected
        }
  insert releaseArtistNames | V.null releaseArtistNames = pure V.empty
  insert releaseArtistNames = do
    pool <- getConnectionPool
    V.fromList
      <$> runInsert
        pool
        Rel8.Insert
          { into = releaseArtistNameSchema,
            rows = Rel8.values (from <$> releaseArtistNames),
            onConflict = Rel8.DoNothing,
            returning = Rel8.Projection (\x -> x)
          }

releaseArtistForReleaseRef :: Expr ReleaseRef -> Query (ReleaseArtistNameTable Expr)
releaseArtistForReleaseRef releaseRef =
  Rel8.filter (\releaseArtist -> releaseArtist.release_id ==. releaseRef) =<< Rel8.each releaseArtistNameSchema

artistNameForReleaseRef :: Expr ReleaseRef -> Query (ArtistNameTable Expr)
artistNameForReleaseRef releaseRef = do
  releaseArtist <- releaseArtistForReleaseRef releaseRef
  Rel8.filter (\artistName -> artistName.id ==. releaseArtist.artist_name_id) =<< Rel8.each artistNameSchema

releaseArtistNameSchema :: Rel8.TableSchema (ReleaseArtistNameTable Rel8.Name)
releaseArtistNameSchema =
  Rel8.TableSchema
    { name = "release_artist_name",
      schema = Nothing,
      columns =
        ReleaseArtistNameTable
          { release_id = "release_id",
            artist_name_id = "artist_name_id"
          }
    }
