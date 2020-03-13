{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Genre.Repo where

import Control.Algebra
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import GHC.Generics (Generic1)
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewGenre
  = NewGenre
      { name :: Text,
        description :: Maybe Text
      }
  deriving (Generic, Eq, Show)

data GenreRepository m k
  = GetAllGenres ([DB.Genre] -> m k)
  | GetGenresById [DB.GenreKey] ([DB.Genre] -> m k)
  | GetGenresByName [Text] ([DB.Genre] -> m k)
  | SearchGenres Text ([DB.Genre] -> m k)
  | GetGenreArtists [DB.GenreKey] ([(DB.GenreKey, DB.Artist)] -> m k)
  | GetGenreAlbums [DB.GenreKey] ([(DB.GenreKey, DB.Album)] -> m k)
  | GetGenreTracks [DB.GenreKey] ([(DB.GenreKey, DB.Track)] -> m k)
  | InsertGenres [NewGenre] ([DB.GenreKey] -> m k)
  | DeleteGenres [DB.GenreKey] (m k)
  deriving (Functor, Generic1, HFunctor, Effect)

getAllGenres :: Has GenreRepository sig m => m [DB.Genre]
getAllGenres = send (GetAllGenres pure)

getGenresById :: Has GenreRepository sig m => [DB.GenreKey] -> m [DB.Genre]
getGenresById ks = send (GetGenresById ks pure)

getGenresByName :: Has GenreRepository sig m => [Text] -> m [DB.Genre]
getGenresByName ns = send (GetGenresByName ns pure)

searchGenres :: Has GenreRepository sig m => Text -> m [DB.Genre]
searchGenres t = send (SearchGenres t pure)

getGenreArtists :: Has GenreRepository sig m => [DB.GenreKey] -> m [(DB.GenreKey, DB.Artist)]
getGenreArtists ks = send (GetGenreArtists ks pure)

getGenreAlbums :: Has GenreRepository sig m => [DB.GenreKey] -> m [(DB.GenreKey, DB.Album)]
getGenreAlbums ks = send (GetGenreAlbums ks pure)

getGenreTracks :: Has GenreRepository sig m => [DB.GenreKey] -> m [(DB.GenreKey, DB.Track)]
getGenreTracks ks = send (GetGenreTracks ks pure)

insertGenres :: Has GenreRepository sig m => [NewGenre] -> m [DB.GenreKey]
insertGenres gs = send (InsertGenres gs pure)

deleteGenres :: Has GenreRepository sig m => [DB.GenreKey] -> m ()
deleteGenres ks = send (DeleteGenres ks (pure ()))

newtype GenreRepositoryIOC m a
  = GenreRepositoryIOC
      { runGenreRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig) =>
  Algebra (GenreRepository :+: sig) (GenreRepositoryIOC m)
  where
  alg = \case
    L (GetAllGenres k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select (all_ (DB.libraryDb ^. #genre))
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (GetGenresById [] k) -> pure [] >>= k
    L (GetGenresById ks k) -> GenreRepositoryIOC $ do
      conn <- ask
      let genreIds = fmap (\(DB.GenreKey gk) -> val_ gk) ks
      let q = select $ filter_ (\g -> g ^. #id `in_` genreIds) (all_ (DB.libraryDb ^. #genre))
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (GetGenresByName [] k) -> pure [] >>= k
    L (GetGenresByName ns k) -> GenreRepositoryIOC $ do
      conn <- ask
      let names = fmap val_ ns
      let q = select $ filter_ (\g -> g ^. #name `in_` names) (all_ (DB.libraryDb ^. #genre))
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (SearchGenres "" k) -> pure [] >>= k
    L (SearchGenres t k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            genre <- all_ (DB.libraryDb ^. #genre)
            guard_ (toTsVector Nothing (genre ^. #name) @@ toTsQuery Nothing (val_ (t <> "|" <> t <> ":*")))
            pure genre
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (GetGenreArtists [] k) -> pure [] >>= k
    L (GetGenreArtists ks k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            let genreIds = fmap (\(DB.GenreKey gk) -> val_ gk) ks
            (g, a) <-
              manyToMany_
                (DB.libraryDb ^. #artist_genre)
                (^. #genre_id)
                (^. #artist_id)
                ( filter_
                    (\g -> g ^. #id `in_` genreIds)
                    (all_ (DB.libraryDb ^. #genre))
                )
                (all_ (DB.libraryDb ^. #artist))
            pure (primaryKey g, a)
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (GetGenreAlbums [] k) -> pure [] >>= k
    L (GetGenreAlbums ks k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            let genreIds = fmap (\(DB.GenreKey gk) -> val_ gk) ks
            (g, a) <-
              manyToMany_
                (DB.libraryDb ^. #album_genre)
                (^. #genre_id)
                (^. #album_id)
                ( filter_
                    (\g -> g ^. #id `in_` genreIds)
                    (all_ (DB.libraryDb ^. #genre))
                )
                (all_ (DB.libraryDb ^. #album))
            pure (primaryKey g, a)
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (GetGenreTracks [] k) -> pure [] >>= k
    L (GetGenreTracks ks k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            let genreIds = fmap (\(DB.GenreKey gk) -> val_ gk) ks
            (g, t) <-
              manyToMany_
                (DB.libraryDb ^. #track_genre)
                (^. #genre_id)
                (^. #track_id)
                ( filter_
                    (\g -> g ^. #id `in_` genreIds)
                    (all_ (DB.libraryDb ^. #genre))
                )
                (all_ (DB.libraryDb ^. #track))
            pure (primaryKey g, t)
      runGenreRepositoryIOC $ runPgDebug conn (runSelectReturningList q) >>= k
    L (InsertGenres [] k) -> pure [] >>= k
    L (InsertGenres gs k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q =
            insertReturning
              (DB.libraryDb ^. #genre)
              (insertExpressions (newGenres <$> gs))
              (onConflict (conflictingFields (^. #name)) onConflictDoNothing)
              (Just primaryKey)
      runGenreRepositoryIOC $ runPgDebug conn (runPgInsertReturningList q) >>= k
    L (DeleteGenres [] k) -> pure () >> k
    L (DeleteGenres ks k) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = delete (DB.libraryDb ^. #genre) (\g -> g ^. #id `in_` fmap (\(DB.GenreKey gk) -> val_ gk) ks)
      runGenreRepositoryIOC $ runPgDebug conn (runDelete q) >> k
    R other -> GenreRepositoryIOC (alg (R (handleCoercible other)))

newGenres :: NewGenre -> DB.GenreT (QExpr Postgres s)
newGenres g = DB.Genre
  { id = default_,
    name = val_ $ g ^. #name,
    description = val_ $ g ^. #description
  }

runGenreRepositoryIO :: Connection -> GenreRepositoryIOC m a -> m a
runGenreRepositoryIO conn = runReader conn . runGenreRepositoryIOC
