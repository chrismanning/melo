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
import qualified Melo.Database.Model as DB
import Melo.Database.Query

data NewGenre = NewGenre
  { name :: Text,
    description :: Maybe Text
  }
  deriving (Generic, Eq, Show)

data GenreRepository :: Effect where
  GetAllGenres :: GenreRepository m [DB.Genre]
  GetGenres :: [DB.GenreKey] -> GenreRepository m [DB.Genre]
  GetGenresByName :: [Text] -> GenreRepository m [DB.Genre]
  SearchGenres :: Text -> GenreRepository m [DB.Genre]
  GetGenreArtists :: [DB.GenreKey] -> GenreRepository m [(DB.GenreKey, DB.Artist)]
  GetGenreAlbums :: [DB.GenreKey] -> GenreRepository m [(DB.GenreKey, DB.Album)]
  GetGenreTracks :: [DB.GenreKey] -> GenreRepository m [(DB.GenreKey, DB.Track)]
  InsertGenres :: [NewGenre] -> GenreRepository m [DB.GenreKey]
  DeleteGenres :: [DB.GenreKey] -> GenreRepository m ()

getAllGenres :: Has GenreRepository sig m => m [DB.Genre]
getAllGenres = send GetAllGenres

getGenres :: Has GenreRepository sig m => [DB.GenreKey] -> m [DB.Genre]
getGenres ks = send (GetGenres ks)

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

tbl :: DatabaseEntity Postgres DB.MeloDb (TableEntity DB.GenreT)
tbl = DB.meloDb ^. #genre

instance
  (Has (Lift IO) sig m, Has (Reader Connection) sig m, Algebra sig m) =>
  Algebra (GenreRepository :+: sig) (GenreRepositoryIOC m)
  where
  alg hdl sig ctx = case sig of
    L GetAllGenres -> GenreRepositoryIOC $ ctx $$> getAll tbl
    L (GetGenres ks) -> GenreRepositoryIOC $ ctx $$> getByKeys tbl ks
    L (DeleteGenres ks) -> ctx $$> deleteByKeys tbl ks
    L (GetGenresByName []) -> ctx $$> pure []
    L (GetGenresByName ns) -> GenreRepositoryIOC $ do
      conn <- ask
      let names = val_ <$> ns
      let q = select $ filter_ (\g -> g ^. #name `in_` names) (all_ tbl)
      ctx $$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (SearchGenres "") -> ctx $$> pure []
    L (SearchGenres t) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            genre <- all_ (DB.meloDb ^. #genre)
            guard_ (toTsVector Nothing (genre ^. #name) @@ toTsQuery Nothing (val_ (t <> "|" <> t <> ":*")))
            pure genre
      ctx $$> $(runPgDebug') conn (runSelectReturningList q)
    L (GetGenreArtists []) -> ctx $$> pure []
    L (GetGenreArtists ks) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            (g, a) <-
              manyToMany_
                (DB.meloDb ^. #artist_genre)
                (^. #genre_id)
                (^. #artist_id)
                (byKeys tbl ks)
                (all_ (DB.meloDb ^. #artist))
            pure (primaryKey g, a)
      ctx $$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenreAlbums []) -> pure $ ctx $> []
    L (GetGenreAlbums ks) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            (g, a) <-
              manyToMany_
                (DB.meloDb ^. #album_genre)
                (^. #genre_id)
                (^. #album_id)
                (byKeys tbl ks)
                (all_ (DB.meloDb ^. #album))
            pure (primaryKey g, a)
      ctx $$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (GetGenreTracks []) -> pure $ ctx $> []
    L (GetGenreTracks ks) -> GenreRepositoryIOC $ do
      conn <- ask
      let q = select $ do
            (g, t) <-
              manyToMany_
                (DB.meloDb ^. #track_genre)
                (^. #genre_id)
                (^. #track_id)
                (byKeys tbl ks)
                (all_ (DB.meloDb ^. #track))
            pure (primaryKey g, t)
      ctx $$> runGenreRepositoryIOC ($(runPgDebug') conn (runSelectReturningList q))
    L (InsertGenres []) -> pure $ ctx $> []
    L (InsertGenres gs) -> GenreRepositoryIOC $ do
      conn <- ask
      let q =
            insertReturning
              (DB.meloDb ^. #genre)
              (insertExpressions (newGenres <$> gs))
              (onConflict (conflictingFields (^. #name)) onConflictDoNothing)
              (Just primaryKey)
      ctx $$> runGenreRepositoryIOC ($(runPgDebug') conn (runPgInsertReturningList q))
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
