{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Album.ArtistName.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool
import Data.Vector (Vector)
import Data.Vector qualified as V
import Hasql.Connection
import Melo.Database.Repo.IO
import Melo.Library.Album.ArtistName.Types
import Melo.Library.Album.Types (AlbumRef (..))
import Melo.Library.Artist.Name.Repo (artistNameSchema)
import Melo.Library.Artist.Name.Types
  ( ArtistNameEntity,
    ArtistNameTable (..),
  )
import Rel8 (Query, Expr, lit, (==.))
import Rel8 qualified
import Witch

class Monad m => AlbumArtistNameRepository m where
  getAlbumArtistNames :: AlbumRef -> m (Vector ArtistNameEntity)
  insert' :: Vector AlbumArtistNameEntity -> m Int

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    AlbumArtistNameRepository m
  ) =>
  AlbumArtistNameRepository (t m)
  where
  getAlbumArtistNames = lift . getAlbumArtistNames
  insert' = lift . insert'

newtype AlbumArtistNameRepositoryIOT m a = AlbumArtistNameRepositoryIOT
  { runAlbumArtistNameRepositoryIOT :: ReaderT DbConnection m a
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

instance MonadIO m => AlbumArtistNameRepository (AlbumArtistNameRepositoryIOT m) where
  getAlbumArtistNames albumRef = do
    connSrc <- ask
    runSelect connSrc (artistNameForAlbumRef $ lit albumRef)
  insert' albumArtistNames | V.null albumArtistNames = pure 0
  insert' albumArtistNames = do
    connSrc <- ask
    runInsert connSrc
      Rel8.Insert
        { into = albumArtistNameSchema,
          rows = Rel8.values (from <$> albumArtistNames),
          onConflict = Rel8.DoNothing,
          returning = fromIntegral <$> Rel8.NumberOfRowsAffected
        }

albumArtistForAlbumRef :: Expr AlbumRef -> Query (AlbumArtistNameTable Expr)
albumArtistForAlbumRef albumRef =
  Rel8.filter (\albumArtist -> albumArtist.album_id ==. albumRef) =<< Rel8.each albumArtistNameSchema

artistNameForAlbumRef :: Expr AlbumRef -> Query (ArtistNameTable Expr)
artistNameForAlbumRef albumRef = do
  albumArtist <- albumArtistForAlbumRef albumRef
  Rel8.filter (\artistName -> artistName.id ==. albumArtist.artist_name_id) =<< Rel8.each artistNameSchema

albumArtistNameSchema :: Rel8.TableSchema (AlbumArtistNameTable Rel8.Name)
albumArtistNameSchema =
  Rel8.TableSchema
    { name = "album_artist_name",
      schema = Nothing,
      columns =
        AlbumArtistNameTable
          { album_id = "album_id",
            artist_name_id = "artist_name_id"
          }
    }

runAlbumArtistNameRepositoryPooledIO :: Pool Connection -> AlbumArtistNameRepositoryIOT m a -> m a
runAlbumArtistNameRepositoryPooledIO pool =
  flip
    runReaderT
    (Pooled pool)
    . runAlbumArtistNameRepositoryIOT

runAlbumArtistNameRepositoryIO :: Connection -> AlbumArtistNameRepositoryIOT m a -> m a
runAlbumArtistNameRepositoryIO conn =
  flip
    runReaderT
    (Single conn)
    . runAlbumArtistNameRepositoryIOT
