{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Genre.Repo where

import Control.Lens ((^.))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import qualified Melo.Database.Model as DB
import Melo.Database.Query

data NewGenre = NewGenre
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Eq, Show)

class Monad m => GenreRepository m where
  getAllGenres :: m [DB.Genre]
  getGenres :: [DB.GenreKey] -> m [DB.Genre]
  getGenresByName :: [Text] -> m [DB.Genre]
  getGenreArtists :: [DB.GenreKey] -> m [(DB.GenreKey, DB.Artist)]
  getGenreAlbums :: [DB.GenreKey] -> m [(DB.GenreKey, DB.Album)]
  getGenreTracks :: [DB.GenreKey] -> m [(DB.GenreKey, DB.Track)]
  searchGenres :: Text -> m [DB.Genre]
  insertGenres :: [NewGenre] -> m [DB.GenreKey]
  deleteGenres :: [DB.GenreKey] -> m ()

newtype GenreRepositoryIOT m a = GenreRepositoryIOT
  { runGenreRepositoryIOT :: ReaderT Connection m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans, MonadTransControl)

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.GenreT)
tbl = DB.meloDb ^. #genre

instance
  (MonadIO m) =>
  GenreRepository (GenreRepositoryIOT m)
  where
  getAllGenres = GenreRepositoryIOT $
    ReaderT $ \conn ->
      getAllIO conn tbl
  getGenres ks = GenreRepositoryIOT $
    ReaderT $ \conn ->
      getByKeysIO conn tbl ks
  deleteGenres ks = GenreRepositoryIOT $
    ReaderT $ \conn ->
      deleteByKeysIO conn tbl ks
  getGenresByName [] = pure []
  getGenresByName ns = GenreRepositoryIOT $
    ReaderT $ \conn -> do
      let names = val_ <$> ns
      let q = select $ filter_ (\g -> g ^. #name `in_` names) (all_ tbl)
      $(runPgDebugIO') conn (runSelectReturningList q)
  searchGenres "" = pure []
  searchGenres t = GenreRepositoryIOT $
    ReaderT $ \conn -> do
      let q = select $ do
            genre <- all_ (DB.meloDb ^. #genre)
            guard_ (toTsVector Nothing (genre ^. #name) @@ toTsQuery Nothing (val_ (t <> "|" <> t <> ":*")))
            pure genre
      $(runPgDebugIO') conn (runSelectReturningList q)
  getGenreArtists [] = pure []
  getGenreArtists ks = GenreRepositoryIOT $
    ReaderT $ \conn -> do
      let q = select $ do
            (g, a) <-
              manyToMany_
                (DB.meloDb ^. #artist_genre)
                (^. #genre_id)
                (^. #artist_id)
                (byKeys tbl ks)
                (all_ (DB.meloDb ^. #artist))
            pure (primaryKey g, a)
      $(runPgDebugIO') conn (runSelectReturningList q)
  getGenreAlbums [] = pure []
  getGenreAlbums ks = GenreRepositoryIOT $
    ReaderT $ \conn -> do
      let q = select $ do
            (g, a) <-
              manyToMany_
                (DB.meloDb ^. #album_genre)
                (^. #genre_id)
                (^. #album_id)
                (byKeys tbl ks)
                (all_ (DB.meloDb ^. #album))
            pure (primaryKey g, a)
      $(runPgDebugIO') conn (runSelectReturningList q)
  getGenreTracks [] = pure []
  getGenreTracks ks = GenreRepositoryIOT $
    ReaderT $ \conn -> do
      let q = select $ do
            (g, t) <-
              manyToMany_
                (DB.meloDb ^. #track_genre)
                (^. #genre_id)
                (^. #track_id)
                (byKeys tbl ks)
                (all_ (DB.meloDb ^. #track))
            pure (primaryKey g, t)
      $(runPgDebugIO') conn (runSelectReturningList q)
  insertGenres [] = pure []
  insertGenres gs = GenreRepositoryIOT $
    ReaderT $ \conn -> do
      let q =
            insertReturning
              (DB.meloDb ^. #genre)
              (insertExpressions (newGenres <$> gs))
              (onConflict (conflictingFields (^. #name)) onConflictDoNothing)
              (Just primaryKey)
      $(runPgDebugIO') conn (runPgInsertReturningList q)

newGenres :: NewGenre -> DB.GenreT (QExpr Postgres s)
newGenres g =
  DB.Genre
    { id = default_,
      name = val_ $ g ^. #name,
      description = val_ $ g ^. #description
    }

runGenreRepositoryIO :: Connection -> GenreRepositoryIOT m a -> m a
runGenreRepositoryIO conn = flip runReaderT conn . runGenreRepositoryIOT
