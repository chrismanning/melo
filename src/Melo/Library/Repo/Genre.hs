module Melo.Library.Repo.Genre where

import Control.Lens
import Control.Monad
import Data.Bifunctor
import Data.Hashable
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import Data.Typeable
import Database.Beam
import Database.Beam.Postgres
import Haxl.Core
import Melo.API.Haxl
import qualified Melo.Library.Database.Model as BM
import Melo.Library.Database.Query

getAllGenres :: Haxl [BM.Genre]
getAllGenres = dataFetch GetAllGenres

getGenreById :: BM.GenreKey -> Haxl (Maybe BM.Genre)
getGenreById = dataFetch . GetGenreById

getGenreByName :: Text -> Haxl (Maybe BM.Genre)
getGenreByName = dataFetch . GetGenreByName

searchGenres :: Text -> Haxl [BM.Genre]
searchGenres = dataFetch . SearchGenres

getGenreTracks :: BM.GenreKey -> Haxl [BM.Track]
getGenreTracks = dataFetch . GetGenreTracks

data GenreRepo a where
  GetAllGenres :: GenreRepo [BM.Genre]
  GetGenreById :: BM.GenreKey -> GenreRepo (Maybe BM.Genre)
  GetGenreByName :: Text -> GenreRepo (Maybe BM.Genre)
  SearchGenres :: Text -> GenreRepo [BM.Genre]
  GetGenreArtists :: BM.GenreKey -> GenreRepo [BM.Artist]
  GetGenreAlbums :: BM.GenreKey -> GenreRepo [BM.Album]
  GetGenreTracks :: BM.GenreKey -> GenreRepo [BM.Track]
  deriving (Typeable)

deriving instance Eq (GenreRepo a)

instance Hashable (GenreRepo a) where
  hashWithSalt s GetAllGenres = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetGenreById _) = hashWithSalt s (1 :: Int)
  hashWithSalt s (GetGenreByName _) = hashWithSalt s (2 :: Int)
  hashWithSalt s (SearchGenres _) = hashWithSalt s (3 :: Int)
  hashWithSalt s (GetGenreArtists _) = hashWithSalt s (4 :: Int)
  hashWithSalt s (GetGenreAlbums _) = hashWithSalt s (5 :: Int)
  hashWithSalt s (GetGenreTracks _) = hashWithSalt s (6 :: Int)

deriving instance Show (GenreRepo a)

instance ShowP GenreRepo where showp = show

instance StateKey GenreRepo where
  data State GenreRepo = GenreState {
    connPool :: Pool Connection
  } deriving (Generic)

instance DataSourceName GenreRepo where
  dataSourceName = const "GenreDataSource"

instance DataSource () GenreRepo where
  fetch state _flags _ = BackgroundFetch $ \blockedFetches -> do
    getAllGenres' [r | BlockedFetch GetAllGenres r <- blockedFetches]
    searchGenres' $ Map.fromList [(t, r) | BlockedFetch (SearchGenres t) r <- blockedFetches]
    getGenreTracks' $ Map.fromList [(gk, r) | BlockedFetch (GetGenreTracks gk) r <- blockedFetches]
    where
      getAllGenres' :: [ResultVar [BM.Genre]] -> IO ()
      getAllGenres' vs = unless (null vs)
        $ withResource (state ^. #connPool)
        $ \conn -> do
          allGenres <- runBeamPostgresDebug putStrLn conn $ runSelectReturningList (select getAllGenresQuery)
          mapM_ (`putSuccess` allGenres) vs
      searchGenres' :: Map.Map Text (ResultVar [BM.Genre]) -> IO ()
      searchGenres' vs = unless (null vs)
        $ forM_ (Map.toList vs)
        $ \(t, r) ->
          withResource (state ^. #connPool) $ \conn ->
            putSuccess r
              =<< runBeamPostgresDebug
                putStrLn
                conn
                (runSelectReturningList (select $ getGenresQuery t))
      getGenreTracks' :: Map.Map BM.GenreKey (ResultVar [BM.Track]) -> IO ()
      getGenreTracks' vs = unless (null vs)
        $ withResource (state ^. #connPool)
        $ \conn -> do
          genreTracks <-
            runBeamPostgresDebug putStrLn conn $
              runSelectReturningList (select (getGenreTracksQuery (Map.keys vs)))
          let genreTracks' = Map.fromListWith (++) (second (: []) <$> genreTracks)
          mapM_
            ( \(k, r) ->
                putSuccess r (fromMaybe [] (Map.lookup k genreTracks'))
            )
            $ Map.toList vs
