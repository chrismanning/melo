{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Melo.API where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.ByteString.Lazy.Char8
import Data.Morpheus
import Data.Morpheus.Types
import Data.Pool
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Typeable
import Data.UUID
import GHC.Generics hiding (from)
import Hasql.Connection
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Library.API
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import qualified Melo.Library.Source.API as API
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Melo.Library.Source.Types (SourceRef (..))
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
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

rootResolver :: ResolverE m => RootResolver m () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {library = resolveLibrary},
      mutationResolver = Mutation {library = libraryMutation},
      subscriptionResolver = Undefined
    }

gqlApi :: forall m. (ResolverE m, Typeable m) => ByteString -> m ByteString
gqlApi = interpreter (rootResolver)

gqlApiIO :: CollectionWatchState -> Pool Connection -> ByteString -> IO ByteString
gqlApiIO collectionWatchState pool rq = runStdoutLogging do
  $(logInfo) ("handling graphql request" :: T.Text)
  $(logDebug) $ "graphql request: " <> rq
  !rs <- runFileSystemIO $
    runMetadataServiceIO $
      runSourceRepositoryPooledIO pool $
        runFileSystemServiceIO pool $
          runFileSystemWatchServiceIO pool collectionWatchState $
            runCollectionRepositoryPooledIO pool $
              runCollectionServiceIO pool $
                gqlApi rq
  $(logInfo) ("finished handling graphql request" :: T.Text)
  pure rs

type ResolverE m =
  ( MonadIO m,
    Logging m,
    FileSystem m,
    SourceRepository m,
    MetadataService m,
    CollectionRepository m,
    CollectionService m,
    FileSystemService m
  )

--runResolverE ::
--  Pool Connection ->
--  Connection ->
--  CollectionServiceIOT
--    ( CollectionRepositoryIOT
--        ( FileSystemServiceIOT
--            ( SourceRepositoryIOT
--                ( MetadataServiceIOT
--                    ( SavepointIOT
--                        ( LoggingIOT m
--                        )
--                    )
--                )
--            )
--        )
--    )
--    a ->
--  m a
--runResolverE pool conn =
--  runStdoutLogging
--    . runSavepoint conn
--    . runMetadataServiceIO
--    . runSourceRepositoryIO conn
--    . runFileSystemServiceIO
--    . runCollectionRepositoryIO conn
--    . runCollectionServiceIO pool

api :: (MonadIO m, MonadBaseControl IO m) => CollectionWatchState -> Pool Connection -> ScottyT LT.Text m ()
api collectionWatchState pool = do
  middleware (cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))
  matchAny "/api" $ do
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw =<< (liftIO . gqlApiIO collectionWatchState pool =<< body)
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
          Just path -> do
            $(logInfoIO) $ "Found cover image " <> path <> " for source " <> show uuid
            file path
            let fileName' = LT.pack $ takeFileName path
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")

getSourceFilePathIO :: (MonadIO m, MonadBaseControl IO m) => Pool Connection -> SourceRef -> m (Maybe FilePath)
getSourceFilePathIO pool k = withResource pool $ \conn ->
  liftIO $
    runStdoutLogging $
      runFileSystemIO $
        runSourceRepositoryIO conn $
          getSourceFilePath k

findCoverImageIO :: (MonadIO m, MonadBaseControl IO m) => Pool Connection -> SourceRef -> m (Maybe FilePath)
findCoverImageIO pool k = withResource pool $ \conn ->
  liftIO $
    runStdoutLogging $
      runFileSystemIO $
        runSourceRepositoryIO conn $ do
          getSourceFilePath k >>= \case
            Nothing -> do
              $(logWarn) $ "No local file found for source " <> show k
              pure Nothing
            Just path -> do
              $(logInfo) $ "Locating cover image for source " <> show k
              let dir = takeDirectory path
              fmap (\f -> dir <> (pathSeparator : f)) <$> API.findCoverImage dir
