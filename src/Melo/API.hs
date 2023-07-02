{-# LANGUAGE DerivingStrategies #-}

module Melo.API where

import Control.Concurrent.Classy
import Control.Exception (evaluate)
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8
import Data.Morpheus
import Data.Morpheus.Types
import Data.Pool
import Data.Text.Lazy qualified as LT
import Data.UUID (fromASCIIBytes)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Hasql.Connection
import Melo.Common.Config
import Melo.Common.Exception
import Melo.Common.FileSystem
import Melo.Common.FileSystem.Watcher
import Melo.Common.Logging
import Melo.Metadata.Aggregate
import Melo.Common.Monad
import Melo.Common.Uri
import Melo.Common.Uuid
import Melo.Database.Repo.IO (DbConnection(..))
import Melo.Format.Metadata (EmbeddedPicture (..), MetadataFile (..), PictureType (..))
import Melo.Library.API
import Melo.Library.Release.Aggregate
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Release.Repo
import Melo.Library.Artist.Aggregate
import Melo.Library.Artist.Name.Repo
import Melo.Library.Artist.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.FileSystem.Scan
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
import Melo.Lookup.Covers
import Melo.Lookup.MusicBrainz
import Melo.Metadata.API
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Repo
import Network.HTTP.Types.Status
import Network.HTTP.Client as Http
import Network.Wai.Middleware.Cors
import System.FilePath (takeDirectory, takeFileName)
import Web.Scotty.Trans

data Query m = Query
  { library :: m (LibraryQuery m),
    metadata :: m (MetadataQuery m)
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
    { queryResolver = Query
      { library = resolveLibrary,
        metadata = resolveMetadata
      },
      mutationResolver = Mutation {library = libraryMutation}
    }

gqlApi :: forall m. (ResolverE m, Typeable m) => ByteString -> m ByteString
gqlApi = interpreter (rootResolver)

gqlApiIO :: CollectionWatchState -> Pool Connection ->  Http.Manager -> ByteString -> IO ByteString
gqlApiIO collectionWatchState pool httpManager rq = do
  $(logInfo) "handling graphql request"
  $(logDebug) $ "graphql request: " <> showt rq
  let run connSrc =
        runFileSystemIO
          . runConfigRepositoryIO connSrc
          . runFileSystemWatcherIO pool collectionWatchState httpManager
          . runMetadataAggregateIO
          . runSourceRepositoryIO connSrc
          . runTagMappingRepositoryIO connSrc
          . runReleaseRepositoryIO connSrc
          . runReleaseArtistNameRepositoryIO connSrc
          . runArtistNameRepositoryIO connSrc
          . runArtistRepositoryIO connSrc
          . runTrackArtistNameRepositoryIO connSrc
          . runTrackRepositoryIO connSrc
          . runMusicBrainzServiceIO httpManager
          . runCachingMusicBrainzService
          . runMultiTrackIO
          . runCollectionRepositoryIO connSrc
          . runTagMappingAggregate
          . runCollectionAggregateIO pool httpManager collectionWatchState
          . runArtistAggregateIOT
          . runTrackAggregateIOT
          . runReleaseAggregateIOT
          . runSourceAggregateIOT
          . runCoverServiceIO httpManager
  let !isMutation = "mutation" `isPrefixOf` rq
  !rs <- if isMutation then
    withResource pool \conn -> evaluate =<< run (Single conn) (gqlApi rq)
    else
      evaluate =<< run (Pooled pool) (gqlApi rq)
  $(logInfo) "finished handling graphql request"
  pure rs

type ResolverE m =
  ( MonadIO m,
    MonadConc m,
    PrimMonad m,
    Logging m,
    FileSystem m,
    UuidGenerator m,
    TagMappingRepository m,
    TagMappingAggregate m,
    SourceRepository m,
    SourceAggregate m,
    ReleaseAggregate m,
    ArtistAggregate m,
    TrackAggregate m,
    MetadataAggregate m,
    MultiTrack m,
    MusicBrainzService m,
    CollectionRepository m,
    CollectionAggregate m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    ArtistRepository m,
    TrackRepository m,
    TrackArtistNameRepository m,
    CoverService m,
    ConfigService m,
    FileSystemWatcher m
  )

api ::
  (MonadIO m, Logging m, PrimMonad m) =>
  CollectionWatchState ->
  Pool Connection ->
  Http.Manager ->
  ScottyT LT.Text m ()
api collectionWatchState pool httpManager = do
  middleware (cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))
  matchAny "/api" $ do
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw =<< (liftIO . gqlApiIO collectionWatchState pool httpManager =<< body)
  get "/graphiql" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file "graphiql.html"
  get "/source/:id" $ do
    srcId <- param "id"
    case fromASCIIBytes srcId of
      Nothing -> status badRequest400
      Just uuid -> do
        liftIO (getSourceFilePathIO pool (SourceRef uuid)) >>= \case
          Nothing -> do
            $(logWarnIO) $ "No local file found for source " <> showt uuid
            status notFound404
          Just path -> do
            $(logInfoIO) $ "Opening file " <> showt path <> " for streaming"
            file path
            let fileName' = LT.pack $ takeFileName path
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")
  get "/source/:id/image" do
    srcId <- param "id"
    case fromASCIIBytes srcId of
      Nothing -> status badRequest400
      Just uuid -> do
        liftIO (findCoverImageIO pool httpManager collectionWatchState (SourceRef uuid)) >>= \case
          Nothing -> do
            $(logWarnIO) $ "No cover image found for source " <> showt srcId
            status notFound404
          Just (ExternalImageFile path) -> do
            $(logInfoIO) $ "Found cover image " <> showt path <> " for source " <> showt uuid
            file path
            let fileName' = LT.pack $ takeFileName path
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")
          Just (EmbeddedImage pic) -> do
            $(logInfoIO) $ "Found embedded cover image for source " <> showt uuid
            setHeader "Content-Type" (LT.fromStrict pic.mimeType)
            raw (fromStrict pic.pictureData)
  post "/collection/:id/source_groups" do
    collectionId <- param "id"
    mappingNames <- V.fromList <$> param "groupByMappings"
    groupByMappings <-
      runArtistRepositoryIO (Pooled pool) $
        runTagMappingRepositoryIO (Pooled pool) $
          runTagMappingAggregate (getMappingsNamed mappingNames)
    orphans <- rescue (param "orphans") (const $ pure False)
    let filt = if orphans then API.Orphaned else API.AllSourceGroups
    rq <- body
    case fromASCIIBytes collectionId of
      Nothing -> status badRequest400
      Just uuid ->
        stream $
          API.streamSourceGroupsQuery collectionWatchState pool httpManager (CollectionRef uuid) groupByMappings filt rq

getSourceFilePathIO :: Pool Connection -> SourceRef -> IO (Maybe FilePath)
getSourceFilePathIO pool k =
  runFileSystemIO $
    runSourceRepositoryIO (Pooled pool) $
      getSourceFilePath k

data CoverImage = EmbeddedImage EmbeddedPicture | ExternalImageFile FilePath

findCoverImageIO ::
  Pool Connection ->
  Http.Manager ->
  CollectionWatchState ->
  SourceRef -> IO (Maybe CoverImage)
findCoverImageIO pool httpManager cws k = withResource pool $ \conn ->
  runFileSystemIO $ runFileSystemWatcherIO pool cws httpManager $
    runConfigRepositoryIO (Single conn) $
    runSourceRepositoryIO (Single conn) $
      runMetadataAggregateIO do
        getSource k >>= \case
          Nothing -> do
            $(logWarn) $ "No source found for ref " <> showt k
            pure Nothing
          Just src -> case uriToFilePath src.source of
            Nothing -> do
              $(logWarn) $ "No local file found for source " <> showt k
              pure Nothing
            Just path -> do
              $(logInfo) $ "Locating cover image for source " <> showt k <> " at path " <> showt path
              let dir = takeDirectory path
              findCoverImage dir >>= \case
                Just imgPath -> pure $ Just $ ExternalImageFile imgPath
                Nothing -> do
                  openMetadataFile path >>= \case
                    Left e -> do
                      let cause = displayException e
                      $(logWarnV ['cause]) $ "Could not look for embedded image in file " <> showt path
                      pure Nothing
                    Right mf -> do
                      let coverKey = fromMaybe FrontCover src.cover
                      pure $ EmbeddedImage <$> lookup coverKey mf.pictures
