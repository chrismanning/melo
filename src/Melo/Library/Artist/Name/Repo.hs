{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Artist.Name.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Foldl (PrimMonad)
import Control.Lens (firstOf)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Foldable
import Data.Pool
import Data.Text (Text)
import Hasql.Connection
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Types
import Rel8

class Repository ArtistNameEntity m => ArtistNameRepository m where
  getArtistNames :: ArtistRef -> m [Text]
  getAlias :: ArtistRef -> Text -> m (Maybe ArtistNameEntity)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ArtistNameRepository m
  ) =>
  ArtistNameRepository (t m)
  where
  getArtistNames = lift . getArtistNames
  getAlias ref name = lift (getAlias ref name)

newtype ArtistNameRepositoryIOT m a = ArtistNameRepositoryIOT
  { runArtistNameRepositoryIOT :: RepositoryIOT ArtistNameTable m a
  }
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader (RepositoryHandle ArtistNameTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      PrimMonad,
      Repository (ArtistNameTable Result)
    )

instance MonadIO m => ArtistNameRepository (ArtistNameRepositoryIOT m) where
  getArtistNames artistRef = do
    RepositoryHandle {connSrc, tbl} <- ask
    toList <$> runSelect connSrc do
      artistName <- Rel8.each tbl
      Rel8.where_ $ artistName.artist_id ==. lit artistRef
      pure artistName.name
  getAlias artistRef name = do
    RepositoryHandle {connSrc, tbl} <- ask
    firstOf traverse <$> runSelect connSrc do
      artistName <- Rel8.each tbl
      Rel8.where_ $
          artistName.artist_id ==. lit artistRef
        &&. artistName.name ==. lit name
      pure artistName

artistNameSchema :: TableSchema (ArtistNameTable Name)
artistNameSchema =
  TableSchema
    { name = "artist_name",
      schema = Nothing,
      columns =
        ArtistNameTable
          { id = "id",
            artist_id = "artist_id",
            name = "name"
          }
    }

runArtistNameRepositoryPooledIO :: Pool Connection -> ArtistNameRepositoryIOT m a -> m a
runArtistNameRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = artistNameSchema,
        pk = (\t -> t.id),
        upsert = Nothing
      }
    . runRepositoryIOT
    . runArtistNameRepositoryIOT

runArtistNameRepositoryIO :: Connection -> ArtistNameRepositoryIOT m a -> m a
runArtistNameRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = artistNameSchema,
        pk = (\t -> t.id),
        upsert = Nothing
      }
    . runRepositoryIOT
    . runArtistNameRepositoryIOT
