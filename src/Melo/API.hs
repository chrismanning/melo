{-# LANGUAGE DerivingStrategies #-}

module Melo.API where

import Control.Concurrent.Classy
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.ByteString.Lazy.Char8
import Data.Maybe
import Data.Morpheus
import Data.Morpheus.Types
import Data.Pool
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.UUID (fromASCIIBytes)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Hasql.Connection
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Format.Metadata (EmbeddedPicture (..), MetadataFile (..), PictureType (..))
import Melo.Library.API
import Melo.Library.Album.Aggregate
import Melo.Library.Album.ArtistName.Repo
import Melo.Library.Album.Repo
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.FileSystem.Watcher
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types (CollectionRef (..))
import Melo.Library.Source.API qualified as API
import Melo.Library.Source.Aggregate
import Melo.Library.Source.MultiTrack
import Melo.Library.Source.Repo as Source
import Melo.Library.Source.Types (Source (..), SourceRef (..))
import Melo.Library.Track.Aggregate
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.MusicBrainz
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
import Network.Wreq.Session qualified as Wreq
import System.FilePath (pathSeparator, takeDirectory, takeFileName)
import Web.Scotty.Trans

data Query m = Query
  { library :: m (LibraryQuery m)
  }
  deriving (Generic)

instance Typeable m => GQLType (Query m)

data Mutation m = Mutation
  { library :: m (LibraryMutation m)
  }
  deriving (Generic)

instance Typeable m => GQLType (Mutation m)

-- data Subscription m = Subscription
--  {
--  library :: m (LibrarySubscription m)
--  }
--  deriving (Generic)
--
-- instance Typeable m => GQLType (Subscription m)

rootResolver :: ResolverE m => RootResolver m () Query Mutation Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {library = resolveLibrary},
      mutationResolver = Mutation {library = libraryMutation}
    }

gqlApi :: forall m. (ResolverE m, Typeable m) => ByteString -> m ByteString
gqlApi = interpreter (rootResolver)

gqlApiIO :: CollectionWatchState -> Pool Connection -> Wreq.Session -> ByteString -> IO ByteString
gqlApiIO collectionWatchState pool sess rq = runStdoutLogging do
  $(logInfo) ("handling graphql request" :: T.Text)
  $(logDebug) $ "graphql request: " <> rq
  let run =
        runFileSystemIO
          . runMetadataAggregateIO
          . runSourceRepositoryPooledIO pool
          . runTagMappingRepositoryPooledIO pool
          . runAlbumRepositoryPooledIO pool
          . runAlbumArtistNameRepositoryPooledIO pool
          . runArtistNameRepositoryPooledIO pool
          . runArtistRepositoryPooledIO pool
          . runTrackArtistNameRepositoryPooledIO pool
          . runTrackRepositoryPooledIO pool
          . runMusicBrainzServiceUnlimitedIO sess
          . runFileSystemWatcherIO pool collectionWatchState sess
          . runMultiTrackIO
          . runCollectionRepositoryPooledIO pool
          . runTagMappingAggregate
          . runCollectionAggregateIO pool sess
          . runArtistAggregateIOT
          . runTrackAggregateIOT
          . runAlbumAggregateIOT
          . runSourceAggregateIOT
  !rs <- run (gqlApi rq)
  $(logInfo) ("finished handling graphql request" :: T.Text)
  pure rs

type ResolverE m =
  ( MonadIO m,
    MonadConc m,
    PrimMonad m,
    Logging m,
    FileSystem m,
    TagMappingRepository m,
    TagMappingAggregate m,
    SourceRepository m,
    SourceAggregate m,
    AlbumAggregate m,
    ArtistAggregate m,
    TrackAggregate m,
    MetadataAggregate m,
    MultiTrack m,
    MusicBrainzService m,
    CollectionRepository m,
    CollectionAggregate m,
    AlbumRepository m,
    AlbumArtistNameRepository m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackRepository m,
    TrackArtistNameRepository m,
    FileSystemWatcher m
  )

api ::
  (MonadIO m, MonadBaseControl IO m, Logging m, PrimMonad m) =>
  CollectionWatchState ->
  Pool Connection ->
  Wreq.Session ->
  ScottyT LT.Text m ()
api collectionWatchState pool sess = do
  middleware (cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))
  matchAny "/api" $ do
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw =<< (liftIO . gqlApiIO collectionWatchState pool sess =<< body)
  get "/graphiql" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file "graphiql.html"
  get "/source/:id" $ do
    srcId <- param "id"
    case fromASCIIBytes srcId of
      Nothing -> status badRequest400
      Just uuid -> do
        getSourceFilePathIO pool (SourceRef uuid) >>= \case
          Nothing -> do
            $(logWarnIO) $ "No local file found for source " <> show uuid
            status notFound404
          Just path -> do
            $(logInfoIO) $ "Opening file '" <> path <> "' for streaming"
            file path
            let fileName' = LT.pack $ takeFileName path
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")
  get "/source/:id/image" do
    srcId <- param "id"
    case fromASCIIBytes srcId of
      Nothing -> status badRequest400
      Just uuid -> do
        findCoverImageIO pool (SourceRef uuid) >>= \case
          Nothing -> do
            $(logWarnIO) $ "No cover image found for source " <> srcId
            status notFound404
          Just (ExternalImageFile path) -> do
            $(logInfoIO) $ "Found cover image " <> path <> " for source " <> show uuid
            file path
            let fileName' = LT.pack $ takeFileName path
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")
          Just (EmbeddedImage pic) -> do
            $(logInfoIO) $ "Found embedded cover image for source " <> show uuid
            setHeader "Content-Type" (LT.fromStrict pic.mimeType)
            raw (fromStrict pic.pictureData)
  post "/collection/:id/source_groups" do
    collectionId <- param "id"
    mappingNames <- V.fromList <$> param "groupByMappings"
    groupByMappings <-
      runArtistRepositoryPooledIO pool $
        runTagMappingRepositoryPooledIO pool $
          runTagMappingAggregate (getMappingsNamed mappingNames)
    rq <- body
    case fromASCIIBytes collectionId of
      Nothing -> status badRequest400
      Just uuid ->
        stream $
          API.streamSourceGroupsQuery collectionWatchState pool (CollectionRef uuid) groupByMappings rq

instance (PrimMonad m, ScottyError a) => PrimMonad (ActionT a m) where
  type PrimState (ActionT a m) = PrimState m
  primitive = lift . primitive

getSourceFilePathIO :: (MonadIO m, MonadBaseControl IO m) => Pool Connection -> SourceRef -> m (Maybe FilePath)
getSourceFilePathIO pool k = withResource pool $ \conn ->
  liftIO $
    runStdoutLogging $
      runFileSystemIO $
        runSourceRepositoryIO conn $
          getSourceFilePath k

data CoverImage = EmbeddedImage EmbeddedPicture | ExternalImageFile FilePath

findCoverImageIO :: (MonadIO m, MonadBaseControl IO m) => Pool Connection -> SourceRef -> m (Maybe CoverImage)
findCoverImageIO pool k = withResource pool $ \conn ->
  liftIO $
    runStdoutLogging $
      runFileSystemIO $
        runSourceRepositoryIO conn $
          runMetadataAggregateIO do
            getSource k >>= \case
              Nothing -> do
                $(logWarn) $ "No source found for ref " <> show k
                pure Nothing
              Just src -> case uriToFilePath src.source of
                Nothing -> do
                  $(logWarn) $ "No local file found for source " <> show k
                  pure Nothing
                Just path -> do
                  $(logInfo) $ "Locating cover image for source " <> show k <> " at path " <> show path
                  let dir = takeDirectory path
                  findCoverImage dir >>= \case
                    Just imgPath -> pure $ Just $ ExternalImageFile imgPath
                    Nothing -> do
                      openMetadataFile path >>= \case
                        Left e -> do
                          $(logWarn) $ "Could not look for embedded image in file " <> show path <> ": " <> displayException e
                          pure Nothing
                        Right mf -> do
                          let coverKey = fromMaybe FrontCover src.cover
                          pure $ EmbeddedImage <$> lookup coverKey mf.pictures
