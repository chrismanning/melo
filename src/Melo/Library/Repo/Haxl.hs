module Melo.Library.Repo.Haxl where

import Control.Carrier.Reader
import Control.Lens ((^.))
import Control.Monad
import Data.Bifunctor
import qualified Data.HashMap.Strict as H
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Database.PostgreSQL.Simple
import GHC.Generics
import Haxl.Core
import qualified Melo.Library.Album.Repo as Album
import qualified Melo.Library.Artist.Repo as Artist
import qualified Melo.Library.Database.Model as DB
import qualified Melo.Library.Genre.Repo as Genre
import qualified Melo.Library.Metadata.Repo as Metadata
import qualified Melo.Library.Track.Repo as Track
import Network.URI

type Haxl = GenHaxl () ()

-- MetadataSource

getMetadataSource :: DB.MetadataSourceKey -> Haxl (Maybe DB.MetadataSource)
getMetadataSource = dataFetch . GetMetadataSource

getFileMetadataSourceBySrc :: FilePath -> Haxl [DB.MetadataSource]
getFileMetadataSourceBySrc = dataFetch . GetFileMetadataSourceBySrc

data MetadataSourceDataSource a where
  GetMetadataSource :: DB.MetadataSourceKey -> MetadataSourceDataSource (Maybe DB.MetadataSource)
  GetFileMetadataSourceBySrc :: FilePath -> MetadataSourceDataSource [DB.MetadataSource]
  deriving (Typeable)

deriving instance Eq (MetadataSourceDataSource a)

instance Hashable (MetadataSourceDataSource a) where
  hashWithSalt s (GetMetadataSource _) = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetFileMetadataSourceBySrc _) = hashWithSalt s (1 :: Int)

deriving instance Show (MetadataSourceDataSource a)

instance ShowP MetadataSourceDataSource where showp = show

instance StateKey MetadataSourceDataSource where
  data State MetadataSourceDataSource
    = MetadataSourceState
        { conn :: Connection
        }
    deriving (Generic)

instance DataSourceName MetadataSourceDataSource where
  dataSourceName = const "MetadataSourceDataSource"

instance DataSource () MetadataSourceDataSource where
  fetch state _flags _ = BackgroundFetch $ \blockedFetches -> do
    getMetadataSource' [(k, r) | BlockedFetch (GetMetadataSource k) r <- blockedFetches]
    getFileMetadataSourcesBySrc' [(Metadata.fileUri p, r) | BlockedFetch (GetFileMetadataSourceBySrc p) r <- blockedFetches]
    where
      getMetadataSource' :: [(DB.MetadataSourceKey, ResultVar (Maybe DB.MetadataSource))] -> IO ()
      getMetadataSource' vs = unless (null vs) $
        do
          allMetadataSources <-
            runReader conn $ Metadata.runMetadataSourceRepositoryIO $
              Metadata.getMetadataSources (fmap fst vs)
          let ms = H.fromList $ fmap (\m -> (DB.MetadataSourceKey $ m ^. #id, m)) allMetadataSources
          forM_ vs $ \(k, v) ->
            case H.lookup k ms of
              Just m -> putSuccess v (Just m)
              Nothing -> putSuccess v Nothing
      getFileMetadataSourcesBySrc' :: [(URI, ResultVar [DB.MetadataSource])] -> IO ()
      getFileMetadataSourcesBySrc' vs = unless (null vs) $
        do
          allMetadataSources <-
            runReader conn $ Metadata.runMetadataSourceRepositoryIO $
              Metadata.getMetadataSourcesBySrc (fmap fst vs)
          let msm = H.fromListWith (++) $ fmap (\ms -> (T.unpack (ms ^. #source), [ms])) allMetadataSources
          forM_ vs $ \(s, v) ->
            case H.lookup (show s) msm of
              Nothing -> putSuccess v []
              Just ms -> putSuccess v ms
      conn = state ^. #conn

-- Genre

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
  data State GenreDataSource
    = GenreState
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
          allGenres <- runReader conn $ Genre.runGenreRepositoryIO Genre.getAllGenres
          mapM_ (`putSuccess` allGenres) vs
      searchGenres' vs = unless (null vs)
        $ forM_ (H.toList vs)
        $ \(t, r) -> putSuccess r =<< runReader conn (Genre.runGenreRepositoryIO (Genre.searchGenres t))
      getGenreTracks' vs = unless (null vs) $
        do
          genreTracks <- runReader conn (Genre.runGenreRepositoryIO $ Genre.getGenreTracks (H.keys vs))
          let genreTracks' = H.fromListWith (++) (second (: []) <$> genreTracks)
          mapM_
            ( \(k, r) ->
                putSuccess r (fromMaybe [] (H.lookup k genreTracks'))
            )
            $ H.toList vs
      conn = state ^. #conn

-- Track

getTrack :: DB.TrackKey -> Haxl (Maybe DB.Track)
getTrack = dataFetch . GetTrack

getTrackByTitle :: Text -> Haxl [DB.Track]
getTrackByTitle = dataFetch . GetTrackByTitle

searchTracks :: Text -> Haxl [DB.Track]
searchTracks = dataFetch . SearchTracks

data TrackRepo a where
  GetTrack :: DB.TrackKey -> TrackRepo (Maybe DB.Track)
  GetTrackByTitle :: Text -> TrackRepo [DB.Track]
  SearchTracks :: Text -> TrackRepo [DB.Track]

deriving instance Eq (TrackRepo a)

instance Hashable (TrackRepo a) where
  hashWithSalt s (GetTrack _) = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetTrackByTitle _) = hashWithSalt s (1 :: Int)
  hashWithSalt s (SearchTracks _) = hashWithSalt s (2 :: Int)

deriving instance Show (TrackRepo a)

instance ShowP TrackRepo where showp = show

instance StateKey TrackRepo where
  data State TrackRepo
    = TrackRepoState
        { conn :: Connection
        }
    deriving (Generic)

instance DataSourceName TrackRepo where
  dataSourceName = const "TrackDataSource"

instance DataSource () TrackRepo where
  fetch state _flags _ = BackgroundFetch $ \blockedFetches -> do
    undefined
