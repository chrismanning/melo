{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Genre.Repo where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens ((^.))
import Data.Functor
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Full
import Melo.Common.Effect
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query

data NewGenre = NewGenre
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Eq, Show)

data GenreRepository :: Effect where
  GetAllGenres :: GenreRepository m [DB.Genre]
  GetGenresById :: [DB.GenreKey] -> GenreRepository m [DB.Genre]
  GetGenresByName :: [Text] -> GenreRepository m [DB.Genre]
  SearchGenres :: Text -> GenreRepository m [DB.Genre]
  GetGenreArtists :: [DB.GenreKey] -> GenreRepository m [(DB.GenreKey, DB.Artist)]
  GetGenreAlbums :: [DB.GenreKey] -> GenreRepository m [(DB.GenreKey, DB.Album)]
  GetGenreTracks :: [DB.GenreKey] -> GenreRepository m [(DB.GenreKey, DB.Track)]
  InsertGenres :: [NewGenre] -> GenreRepository m [DB.GenreKey]
  DeleteGenres :: [DB.GenreKey] -> GenreRepository m ()

getAllGenres :: Has GenreRepository sig m => m [DB.Genre]
getAllGenres = send GetAllGenres

getGenresById :: Has GenreRepository sig m => [DB.GenreKey] -> m [DB.Genre]
getGenresById ks = send (GetGenresById ks)

getGenresByName :: Has GenreRepository sig m => [Text] -> m [DB.Genre]
getGenresByName ns = send (GetGenresByName ns)

searchGenres :: Has GenreRepository sig m => Text -> m [DB.Genre]
searchGenres t = send (SearchGenres t)

getGenreArtists :: Has GenreRepository sig m => [DB.GenreKey] -> m [(DB.GenreKey, DB.Artist)]
getGenreArtists ks = send (GetGenreArtists ks)

getGenreAlbums :: Has GenreRepository sig m => [DB.GenreKey] -> m [(DB.GenreKey, DB.Album)]
getGenreAlbums ks = send (GetGenreAlbums ks)

getGenreTracks :: Has GenreRepository sig m => [DB.GenreKey] -> m [(DB.GenreKey, DB.Track)]
getGenreTracks ks = send (GetGenreTracks ks)

insertGenres :: Has GenreRepository sig m => [NewGenre] -> m [DB.GenreKey]
insertGenres gs = send (InsertGenres gs)

deleteGenres :: Has GenreRepository sig m => [DB.GenreKey] -> m ()
deleteGenres ks = send (DeleteGenres ks)

newtype GenreRepositoryIOC m a = GenreRepositoryIOC
  { runGenreRepositoryIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

instance
  (Has (Lift IO) sig m, Has (Reader Connection) sig m, Algebra sig m) =>
  Algebra (GenreRepository :+: sig) (GenreRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    L GetAllGenres -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select (all_ (DB.libraryDb ^. #genre))
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenresById []) -> pure $ ctx $> []
    L (GetGenresById ks) -> GenreRepositoryIOC $ do
      conn <- ask
      let genreIds = fmap (\(DB.GenreKey gk) -> val_ gk) ks
      let q = select $ filter_ (\g -> g ^. #id `in_` genreIds) (all_ (DB.libraryDb ^. #genre))
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenresByName []) -> pure $ ctx $> []
    L (GetGenresByName ns) -> GenreRepositoryIOC $ do
      conn <- ask
      let names = fmap val_ ns
      let q = select $ filter_ (\g -> g ^. #name `in_` names) (all_ (DB.libraryDb ^. #genre))
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (SearchGenres "") -> pure $ ctx $> []
    L (SearchGenres t) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            genre <- all_ (DB.libraryDb ^. #genre)
            guard_ (toTsVector Nothing (genre ^. #name) @@ toTsQuery Nothing (val_ (t <> "|" <> t <> ":*")))
            pure genre
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenreArtists []) -> pure $ ctx $> []
    L (GetGenreArtists ks) -> GenreRepositoryIOC $ do
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
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenreAlbums []) -> pure $ ctx $> []
    L (GetGenreAlbums ks) -> GenreRepositoryIOC $ do
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
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenreTracks []) -> pure $ ctx $> []
    L (GetGenreTracks ks) -> GenreRepositoryIOC $ do
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
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (InsertGenres []) -> pure $ ctx $> []
    L (InsertGenres gs) -> GenreRepositoryIOC $ do
      conn <- ask
      let q =
            insertReturning
              (DB.libraryDb ^. #genre)
              (insertExpressions (newGenres <$> gs))
              (onConflict (conflictingFields (^. #name)) onConflictDoNothing)
              (Just primaryKey)
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runPgInsertReturningList q))
    L (DeleteGenres []) -> ctx <$ pure ()
    L (DeleteGenres ks) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = delete (DB.libraryDb ^. #genre) (\g -> g ^. #id `in_` fmap (\(DB.GenreKey gk) -> val_ gk) ks)
      (ctx $>) <$> runGenreRepositoryIOC ($(runPgDebug') conn (runDelete q))
    R other -> GenreRepositoryIOC (alg (runGenreRepositoryIOC . hdl) other ctx)

newGenres :: NewGenre -> DB.GenreT (QExpr Postgres s)
newGenres g =
  DB.Genre
    { id = default_,
      name = val_ $ g ^. #name,
      description = val_ $ g ^. #description
    }

runGenreRepositoryIO :: GenreRepositoryIOC m a -> m a
runGenreRepositoryIO = runGenreRepositoryIOC
