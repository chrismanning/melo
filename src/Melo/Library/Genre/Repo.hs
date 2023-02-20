{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Genre.Repo where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Text (Text)
import Melo.Database.Repo
import Melo.Database.Repo.IO
import Melo.Library.Artist.Types
import Melo.Library.Genre.Types
import Melo.Library.Release.Types
import Melo.Library.Track.Types
import Rel8

class Repository (GenreTable Result) m => GenreRepository m where
  getByName :: [Text] -> m [Genre]
  getArtists :: [GenreRef] -> m [(GenreRef, Artist)]
  getReleases :: [GenreRef] -> m [(GenreRef, Release)]
  getTracks :: [GenreRef] -> m [(GenreRef, Track)]

newtype GenreRepositoryIOT m a = GenreRepositoryIOT
  { runGenreRepositoryIOT :: RepositoryIOT GenreTable m a
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
      MonadReader (RepositoryHandle GenreTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      Repository (GenreTable Result)
    )

--instance
--  (MonadIO m) =>
--  GenreRepository (GenreRepositoryIOT m)
--  where

--  getGenresByName [] = pure []
--  getGenresByName ns = GenreRepositoryIOT $
--    ReaderT $ \conn -> do
--      let names = val_ <$> ns
--      let q = select $ filter_ (\g -> g ^. #name `in_` names) (all_ tbl)
--      $(runPgDebugIO') conn (runSelectReturningList q)
--  searchGenres "" = pure []
--  searchGenres t = GenreRepositoryIOT $
--    ReaderT $ \conn -> do
--      let q = select $ do
--            genre <- all_ (DB.meloDb ^. #genre)
--            guard_ (toTsVector Nothing (genre ^. #name) @@ toTsQuery Nothing (val_ (t <> "|" <> t <> ":*")))
--            pure genre
--      $(runPgDebugIO') conn (runSelectReturningList q)
--  getGenreArtists [] = pure []
--  getGenreArtists ks = GenreRepositoryIOT $
--    ReaderT $ \conn -> do
--      let q = select $ do
--            (g, a) <-
--              manyToMany_
--                (DB.meloDb ^. #artist_genre)
--                (^. #genre_id)
--                (^. #artist_id)
--                (byKeys tbl ks)
--                (all_ (DB.meloDb ^. #artist))
--            pure (primaryKey g, a)
--      $(runPgDebugIO') conn (runSelectReturningList q)
--  getGenreAlbums [] = pure []
--  getGenreAlbums ks = GenreRepositoryIOT $
--    ReaderT $ \conn -> do
--      let q = select $ do
--            (g, a) <-
--              manyToMany_
--                (DB.meloDb ^. #album_genre)
--                (^. #genre_id)
--                (^. #release_id)
--                (byKeys tbl ks)
--                (all_ (DB.meloDb ^. #album))
--            pure (primaryKey g, a)
--      $(runPgDebugIO') conn (runSelectReturningList q)
--  getGenreTracks [] = pure []
--  getGenreTracks ks = GenreRepositoryIOT $
--    ReaderT $ \conn -> do
--      let q = select $ do
--            (g, t) <-
--              manyToMany_
--                (DB.meloDb ^. #track_genre)
--                (^. #genre_id)
--                (^. #track_id)
--                (byKeys tbl ks)
--                (all_ (DB.meloDb ^. #track))
--            pure (primaryKey g, t)
--      $(runPgDebugIO') conn (runSelectReturningList q)

--newGenres :: NewGenre -> DB.GenreT (QExpr Postgres s)
--newGenres g =
--  DB.Genre
--    { id = default_,
--      name = val_ $ g ^. #name,
--      description = val_ $ g ^. #description
--    }
--
--runGenreRepositoryIO :: Connection -> GenreRepositoryIOT m a -> m a
--runGenreRepositoryIO conn = flip runReaderT conn . runGenreRepositoryIOT
