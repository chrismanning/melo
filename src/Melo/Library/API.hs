module Melo.Library.API where

import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encoding as A
import Data.Bifunctor
import Data.Generic.HKD
import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Pool
import Data.Text as T hiding (null)
import Data.Traversable
import Data.Typeable
import Data.UUID (toText)
import Database.Beam hiding (C)
import Database.Beam.Postgres
import GHC.Generics (Generic)
import GHC.OverloadedLabels ()
import Haxl.Core
import Melo.API.Haxl
import Melo.GraphQL.Introspect
import Melo.GraphQL.Resolve
import qualified Melo.Library.Database.Model as BM
import Melo.Library.Database.Query

import Debug.Trace

data Library
  = Library
      { genres :: [Genre],
        artists :: [Artist],
        albums :: [Album],
        tracks :: [Track]
      }
  deriving (Generic)

instance GraphQLType Library where
  type TypeKind Library = 'ObjectKind

data LibraryCtx = LibraryCtx

instance ObjectResolver Haxl Library where
  type ResolverContext Library = LibraryCtx

instance GenericResolver Haxl Library where
  genericResolver = let g = build @Library in
    g (Resolve $ \_ _ fs -> do
        genres <- getAllGenres
        hydratedGenres <- for genres $ \genre ->
          resolveFieldValues genre fs
        traceShowM $ hydratedGenres
        pure $ A.list (\x -> x) hydratedGenres
      )
      (nullresolver)
      (nullresolver)
      (nullresolver)

data Genre
  = Genre
      { id :: Text,
        name :: Text,
        tracks :: [Track]
      }
  deriving (Generic)

instance GraphQLType Genre where
  type TypeKind Genre = 'ObjectKind

instance ObjectResolver Haxl Genre where
  type ResolverContext Genre = BM.Genre

instance GenericResolver Haxl Genre where
  genericResolver = let g = build @Genre in
    g (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #name))
      (Resolve $ \ctx _ fields -> do
        tracks <- getGenreTracks (primaryKey ctx)
        fmap (A.list (\x -> x)) $ forM tracks $ \track ->
          resolveFieldValues track fields
      )

data Artist
  = Artist
      { id :: Text,
        name :: Text,
        biography :: Maybe Text,
        shortBio :: Maybe Text,
        country :: Maybe Text,
        genres :: [Genre],
        albums :: [Album],
        tracks :: [Track]
      } deriving (Generic)

instance GraphQLType Artist where
  type TypeKind Artist = 'ObjectKind

instance ObjectResolver Haxl Artist where
  type ResolverContext Artist = BM.Artist

instance GenericResolver Haxl Artist where
  genericResolver = let a = build @Artist in
    a (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #name))
      (pureCtxResolver (^. #bio))
      (pureCtxResolver (^. #short_bio))
      (pureCtxResolver (^. #country))
      (nullresolver)
      (nullresolver)
      (nullresolver)

data Album
  = Album
      { id :: Text,
        title :: Text
      } deriving (Generic, ToJSON)

instance GraphQLType Album where
  type TypeKind Album = 'ObjectKind

instance ObjectResolver Haxl Album where
  type ResolverContext Album = BM.Album

instance GenericResolver Haxl Album where
  genericResolver = let a = build @Album in
    a (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #title))

data Track
  = Track
      { id :: Text,
        title :: Text
      }
  deriving (Generic, ToJSON)

instance GraphQLType Track where
  type TypeKind Track = 'ObjectKind

instance ObjectResolver Haxl Track where
  type ResolverContext Track = BM.Track

instance GenericResolver Haxl Track where
  genericResolver = let t = build @Track in
    t (pureCtxResolver (toText . (^. #id)))
      (pureCtxResolver (^. #title))

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
