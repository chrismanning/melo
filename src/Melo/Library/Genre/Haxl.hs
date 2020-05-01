module Melo.Library.Genre.Haxl where

import Control.Carrier.Reader
import Control.Lens
import Control.Monad
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import Data.Typeable
import Haxl.Core
import Melo.Common.Haxl
import qualified Melo.Library.Database.Model as DB
import Database.Beam.Postgres (Connection)
import GHC.Generics (Generic)
import qualified Melo.Library.Genre.Repo as Repo
import Data.Bifunctor (second)

getAllGenres :: Haxl [DB.Genre]
getAllGenres = dataFetch GetAllGenres

getGenreById :: DB.GenreKey -> Haxl (Maybe DB.Genre)
getGenreById = dataFetch . GetGenreById

getGenreByName :: Text -> Haxl (Maybe DB.Genre)
getGenreByName = dataFetch . GetGenreByName

searchGenres :: Text -> Haxl [DB.Genre]
searchGenres = dataFetch . SearchGenres

getGenreTracks :: DB.GenreKey -> Haxl [DB.Track]
getGenreTracks = dataFetch . GetGenreTracks

data GenreDataSource a where
  GetAllGenres :: GenreDataSource [DB.Genre]
  GetGenreById :: DB.GenreKey -> GenreDataSource (Maybe DB.Genre)
  GetGenreByName :: Text -> GenreDataSource (Maybe DB.Genre)
  SearchGenres :: Text -> GenreDataSource [DB.Genre]
  GetGenreArtists :: DB.GenreKey -> GenreDataSource [DB.Artist]
  GetGenreAlbums :: DB.GenreKey -> GenreDataSource [DB.Album]
  GetGenreTracks :: DB.GenreKey -> GenreDataSource [DB.Track]
  deriving (Typeable)

deriving instance Eq (GenreDataSource a)

instance Hashable (GenreDataSource a) where
  hashWithSalt s GetAllGenres = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetGenreById _) = hashWithSalt s (1 :: Int)
  hashWithSalt s (GetGenreByName _) = hashWithSalt s (2 :: Int)
  hashWithSalt s (SearchGenres _) = hashWithSalt s (3 :: Int)
  hashWithSalt s (GetGenreArtists _) = hashWithSalt s (4 :: Int)
  hashWithSalt s (GetGenreAlbums _) = hashWithSalt s (5 :: Int)
  hashWithSalt s (GetGenreTracks _) = hashWithSalt s (6 :: Int)

deriving instance Show (GenreDataSource a)

instance ShowP GenreDataSource where showp = show

instance StateKey GenreDataSource where
  data State GenreDataSource = GenreState
    { conn :: Connection
    }
    deriving (Generic)

instance DataSourceName GenreDataSource where
  dataSourceName = const "GenreDataSource"

instance DataSource () GenreDataSource where
  fetch state _flags _ = BackgroundFetch $ \blockedFetches -> do
    getAllGenres' [r | BlockedFetch GetAllGenres r <- blockedFetches]
    searchGenres' $ H.fromList [(t, r) | BlockedFetch (SearchGenres t) r <- blockedFetches]
    getGenreTracks' $ H.fromList [(gk, r) | BlockedFetch (GetGenreTracks gk) r <- blockedFetches]
    where
      getAllGenres' vs = unless (null vs) $
        do
          allGenres <- runReader conn $ Repo.runGenreRepositoryIO Repo.getAllGenres
          mapM_ (`putSuccess` allGenres) vs
      searchGenres' vs = unless (null vs)
        $ forM_ (H.toList vs)
        $ \(t, r) -> putSuccess r =<< runReader conn (Repo.runGenreRepositoryIO (Repo.searchGenres t))
      getGenreTracks' vs = unless (null vs) $
        do
          genreTracks <- runReader conn (Repo.runGenreRepositoryIO $ Repo.getGenreTracks (H.keys vs))
          let genreTracks' = H.fromListWith (++) (second (: []) <$> genreTracks)
          mapM_
            ( \(k, r) ->
                putSuccess r (fromMaybe [] (H.lookup k genreTracks'))
            )
            $ H.toList vs
      conn = state ^. #conn
