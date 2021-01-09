{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Melo.API where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8
import qualified Data.HashMap.Strict as H
import Data.Morpheus
import Data.Morpheus.Types
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Typeable
import Data.UUID
import Database.Beam.Postgres (Connection)
import GHC.Generics
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import qualified Melo.Database.Model as DB
import Melo.Database.Transaction
import Melo.Library.API
import Melo.Library.Collection.FileSystem.Service
import Melo.Library.Collection.FileSystem.WatchService
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import qualified Melo.Library.Source.API as API
import Melo.Library.Source.Repo
import Melo.Library.Source.Service
import Network.HTTP.Types.Status
import Network.Wai.Middleware.Cors
import qualified System.FSNotify as FS
import System.FilePath (takeFileName, takeDirectory, pathSeparator)
import Web.Scotty.Trans
import Control.Monad.Trans.Control (MonadBaseControl)

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

rootResolver :: ResolverE sig m => RootResolver m () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {library = resolveLibrary},
      mutationResolver = Mutation {library = libraryMutation},
      subscriptionResolver = Undefined
    }

gqlApi :: forall sig m. (ResolverE sig m, Typeable m) => ByteString -> m ByteString
gqlApi = interpreter (rootResolver @sig @m)

gqlApiIO :: TVar (H.HashMap CollectionRef FS.StopListening) -> Pool Connection -> ByteString -> IO ByteString
gqlApiIO collectionWatchState pool rq = runStdoutLogging do
  $(logInfo) ("handling graphql request" :: T.Text)
  $(logDebug) $ "graphql request: " <> rq
  rs <- runReader pool $
    runReader collectionWatchState $
      runFileSystemIO $
        runFileSystemWatchServiceIO $
          runTransaction $
            withTransaction $ \conn ->
              runResolverE conn $
                gqlApi rq
  $(logInfo) ("finished handling graphql request" :: T.Text)
  pure rs

type ResolverE sig m =
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m,
    Has Transaction sig m,
    Has Savepoint sig m,
    Has Logging sig m,
    Has FileSystem sig m,
    Has SourceRepository sig m,
    Has MetadataService sig m,
    Has CollectionRepository sig m,
    Has CollectionService sig m,
    Has FileSystemService sig m
  )

runResolverE ::
  (Has (Lift IO) sig m) =>
  t ->
  CollectionServiceIOC
    ( CollectionRepositoryIOC
        ( FileSystemServiceIOC
            ( SourceRepositoryIOC
                ( MetadataServiceIOC
                    ( SavepointC
                        ( LoggingIOC
                            ( ReaderC t m
                            )
                        )
                    )
                )
            )
        )
    )
    a ->
  m a
runResolverE conn =
  runReader conn
    . runStdoutLogging
    . runSavepoint
    . runMetadataServiceIO
    . runSourceRepositoryIO
    . runFileSystemServiceIO
    . runCollectionRepositoryIO
    . runCollectionServiceIO

api :: (MonadIO m, MonadBaseControl IO m) => TVar (H.HashMap CollectionRef FS.StopListening) -> Pool Connection -> ScottyT LT.Text m ()
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
        getSourceFilePathIO pool (DB.SourceKey uuid) >>= \case
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
        findCoverImageIO pool (DB.SourceKey uuid) >>= \case
          Nothing -> do
            $(logWarnIO) $ "No cover image found for source " <> srcId
            status notFound404
          Just path -> do
            $(logInfoIO) $ "Found cover image " <> path <> " for source " <> show uuid
            file path
            let fileName' = LT.pack $ takeFileName path
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")

getSourceFilePathIO :: (MonadIO m, MonadBaseControl IO m) => Pool Connection -> DB.SourceKey -> m (Maybe FilePath)
getSourceFilePathIO pool k = withResource pool $ \conn ->
  liftIO $
    runReader conn $
      runStdoutLogging $
        runFileSystemIO $
          runSourceRepositoryIO $
            getSourceFilePath k

findCoverImageIO :: (MonadIO m, MonadBaseControl IO m) => Pool Connection -> DB.SourceKey -> m (Maybe FilePath)
findCoverImageIO pool k = withResource pool $ \conn ->
  liftIO $
    runReader conn $
      runStdoutLogging $
        runFileSystemIO $
          runSourceRepositoryIO $ do
            getSourceFilePath k >>= \case
              Nothing -> do
                $(logWarn) $ "No local file found for source " <> show k
                pure Nothing
              Just path -> do
                $(logInfo) $ "Locating cover image for source " <> show k
                let dir = takeDirectory path
                fmap (\f -> dir <> (pathSeparator : f)) <$> API.findCoverImage dir
