{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnboxedTuples #-}

module Melo.Library.Artist.Name.Repo where

import Control.Concurrent.Classy
import Melo.Common.Exception
import Control.Foldl (PrimMonad)
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Types
import Rel8

class Repository ArtistNameEntity m => ArtistNameRepository m where
  getArtistNames :: ArtistRef -> m (Vector ArtistNameEntity)
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
    runSelect connSrc do
      artistName <- Rel8.each tbl
      Rel8.where_ $ artistName.artist_id ==. lit artistRef
      pure artistName
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

runArtistNameRepositoryIO :: DbConnection -> ArtistNameRepositoryIOT m a -> m a
runArtistNameRepositoryIO connSrc =
  flip
    runReaderT
    RepositoryHandle
      { connSrc,
        tbl = artistNameSchema,
        pk = (\t -> t.id),
        upsert = Just Upsert {
          index = \t -> (t.artist_id, t.name),
          set = \_new old -> old,
          updateWhere = \_new _old -> lit True
        }
      }
    . runRepositoryIOT
    . runArtistNameRepositoryIOT
