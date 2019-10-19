module Melo.Library.API where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson as A
import Data.Bifunctor
import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Pool
import Data.Text as T hiding (null)
import Data.Traversable
import Data.Typeable
import Database.Beam hiding (C)
import Database.Beam.Postgres
import GHC.Generics (Generic)
import GHC.OverloadedLabels ()
import Haxl.Core
import Language.GraphQL.AST.Core hiding (Query)
import Melo.API.Haxl
import Melo.GraphQL.Resolve
import qualified Melo.Library.Database.Model as BM
import Melo.Library.Database.Query

data Library m
  = Library
      { genres :: FieldResolver m (QLFieldContext (Library m)) [Genre m]
      }
  deriving (Generic)

instance GraphQlType (Library Haxl) where
  resolver = Library
    { genres = \_ _args fields -> do
        genres <- getAllGenres
        hydratedGenres <- for genres $ \genre ->
          resolveFields resolver (GenreFieldContext genre) fields
        pure $ toJSON hydratedGenres
    }

data Genre m
  = Genre
      { id :: FieldResolver m (QLFieldContext (Genre m)) Text,
        name :: FieldResolver m (QLFieldContext (Genre m)) Text,
        tracks :: FieldResolver m (QLFieldContext (Genre m)) [Track m]
      }
  deriving (Generic)

instance GraphQlType (Genre Haxl) where
  resolver = Genre
    { id = \(GenreFieldContext bg) _ _ -> pure $ toJSON $ bg ^. #id,
      name = \(GenreFieldContext bg) _ _ -> pure $ toJSON $ bg ^. #name,
      tracks = \(GenreFieldContext bg) args fields -> toJSON <$> resolveGenreTracks resolver (primaryKey bg) args fields
    }

data Track m
  = Track
      { id :: FieldResolver m (QLFieldContext (Track m)) Text,
        title :: FieldResolver m (QLFieldContext (Track m)) Text
      }
  deriving (Generic)

instance GraphQlType (Track Haxl) where
  resolver = Track
    { id = \(TrackFieldContext bt) _ _ -> pure $ toJSON $ bt ^. #id,
      title = \(TrackFieldContext bt) _ _ -> pure $ toJSON $ bt ^. #title
    }

instance QLFieldResolve Haxl (Library Haxl) where
  data QLFieldContext (Library Haxl) = LibraryFieldContext

instance QLFieldResolve Haxl (Genre Haxl) where
  data QLFieldContext (Genre Haxl) = GenreFieldContext BM.Genre

instance QLFieldResolve Haxl (Track Haxl) where
  data QLFieldContext (Track Haxl) = TrackFieldContext BM.Track

resolveGenreTracks :: Track Haxl -> BM.GenreKey -> [Argument] -> [Field] -> Haxl [A.Value]
resolveGenreTracks t genreKey _args fields = do
  -- TODO args
  tracks <- getGenreTracks genreKey
  for tracks $ \track -> resolveFields t (TrackFieldContext track) fields

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

data GenreDB a where
  GetAllGenres :: GenreDB [BM.Genre]
  GetGenreById :: BM.GenreKey -> GenreDB (Maybe BM.Genre)
  GetGenreByName :: Text -> GenreDB (Maybe BM.Genre)
  SearchGenres :: Text -> GenreDB [BM.Genre]
  GetGenreArtists :: BM.GenreKey -> GenreDB [BM.Artist]
  GetGenreAlbums :: BM.GenreKey -> GenreDB [BM.Album]
  GetGenreTracks :: BM.GenreKey -> GenreDB [BM.Track]
  deriving (Typeable)

deriving instance Eq (GenreDB a)

instance Hashable (GenreDB a) where
  hashWithSalt s GetAllGenres = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetGenreById _) = hashWithSalt s (1 :: Int)
  hashWithSalt s (GetGenreByName _) = hashWithSalt s (2 :: Int)
  hashWithSalt s (SearchGenres _) = hashWithSalt s (3 :: Int)
  hashWithSalt s (GetGenreArtists _) = hashWithSalt s (4 :: Int)
  hashWithSalt s (GetGenreAlbums _) = hashWithSalt s (5 :: Int)
  hashWithSalt s (GetGenreTracks _) = hashWithSalt s (6 :: Int)

deriving instance Show (GenreDB a)

instance ShowP GenreDB where showp = show

instance StateKey GenreDB where
  data State GenreDB = GenreState {}

instance DataSourceName GenreDB where
  dataSourceName = const "GenreDataSource"

instance DataSource Handle GenreDB where
  fetch _state _flags h = BackgroundFetch $ \blockedFetches -> do
    getAllGenres' [r | BlockedFetch GetAllGenres r <- blockedFetches]
    searchGenres' $ Map.fromList [(t, r) | BlockedFetch (SearchGenres t) r <- blockedFetches]
    getGenreTracks' $ Map.fromList [(gk, r) | BlockedFetch (GetGenreTracks gk) r <- blockedFetches]
    where
      getAllGenres' :: [ResultVar [BM.Genre]] -> IO ()
      getAllGenres' vs = unless (null vs)
        $ withResource (h ^. #connPool)
        $ \conn -> do
          allGenres <- runBeamPostgresDebug putStrLn conn $ runSelectReturningList (select getAllGenresQuery)
          mapM_ (\r -> putSuccess r allGenres) vs
      searchGenres' :: Map.Map Text (ResultVar [BM.Genre]) -> IO ()
      searchGenres' vs = unless (null vs)
        $ forM_ (Map.toList vs)
        $ \(t, r) ->
          withResource (h ^. #connPool) $ \conn ->
            putSuccess r
              =<< runBeamPostgresDebug
                putStrLn
                conn
                (runSelectReturningList (select $ getGenresQuery t))
      getGenreTracks' :: Map.Map BM.GenreKey (ResultVar [BM.Track]) -> IO ()
      getGenreTracks' vs = unless (null vs)
        $ withResource (h ^. #connPool)
        $ \conn -> do
          genreTracks <-
            runBeamPostgresDebug putStrLn conn $
              runSelectReturningList (select (getGenreTracksQuery (Map.keys vs)))
          let genreTracks' = Map.fromListWith (++) ((second (: [])) <$> genreTracks)
          mapM_
            ( \(k, r) ->
                putSuccess r (fromMaybe [] (Map.lookup k genreTracks'))
            )
            $ Map.toList vs
