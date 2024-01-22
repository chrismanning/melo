{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Artist.Name.Repo where

import Melo.Common.Monad
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Name.Types
import Melo.Library.Artist.Types
import Rel8

class (Repository ArtistNameEntity m) => ArtistNameRepository m where
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

instance ArtistNameRepository (AppM IO IO) where
  getArtistNames artistRef = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ArtistNameTable
    runSelect pool do
      artistName <- Rel8.each tbl
      Rel8.where_ $ artistName.artist_id ==. lit artistRef
      pure artistName
  getAlias artistRef name = do
    pool <- getConnectionPool
    RepositoryHandle {tbl} <- getRepoHandle @ArtistNameTable
    firstOf traverse <$> runSelect pool do
      artistName <- Rel8.each tbl
      Rel8.where_ $
        artistName.artist_id ==. lit artistRef
          &&. artistName.name ==. lit name
      pure artistName

artistNameSchema :: TableSchema (ArtistNameTable Name)
artistNameSchema =
  TableSchema
    { name = "artist_name",
      columns =
        ArtistNameTable
          { id = "id",
            artist_id = "artist_id",
            name = "name"
          }
    }

initArtistNameRepo :: (AppDataReader m) => m ()
initArtistNameRepo =
  putAppData
    RepositoryHandle
      { tbl = artistNameSchema,
        pk = (\t -> t.id),
        upsert =
          Just
            Upsert
              { index = \t -> (t.artist_id, t.name),
                predicate = Nothing,
                set = \_new old -> old,
                updateWhere = \_new _old -> lit True
              }
      }
