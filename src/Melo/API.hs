{-# LANGUAGE DerivingStrategies #-}

module Melo.API where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8
import Data.Morpheus
import Data.Morpheus.Types
import Data.Pool
import Data.Text.Lazy qualified as LT
import Data.UUID (fromASCIIBytes)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Tracing
import Melo.Common.Uri
import Melo.Database.Repo.IO as DB
import Melo.Format.Metadata (EmbeddedPicture (..), MetadataFile (..), PictureType (..))
import Melo.Library.API
import Melo.Library.Collection.Types (CollectionRef (..))
import Melo.Library.Source.API qualified as API
import Melo.Library.Source.Aggregate
import Melo.Library.Source.Types (Source (..), SourceRef (..))
import Melo.Metadata.Aggregate
import Melo.Metadata.API
import Network.HTTP.Types.Status
import Network.Wai
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

rootResolver :: RootResolver (AppM IO IO) () Query Mutation Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query
      { library = resolveLibrary,
        metadata = resolveMetadata
      },
      mutationResolver = Mutation {library = libraryMutation}
    }

gqlApi :: ByteString -> AppM IO IO ByteString
gqlApi = interpreter (rootResolver)

gqlApiIO :: ByteString -> AppM IO IO ByteString
gqlApiIO rq = do
  $(logInfo) "handling graphql request"
  do
    let request = showt rq
    $(logDebugV ['request]) "graphql request"
  let !isMutation = "mutation" `isPrefixOf` rq
  !rs <- if isMutation then do
    pool <- DB.getConnectionPool
    liftWith (\run -> withResource pool \conn -> do
      pool' <- liftIO $ newPool $ defaultPoolConfig (pure conn) (const (pure ())) 20 1
      run $ localAppData (const $ pure pool') do
        gqlApi rq
      ) >>= restoreT . pure
    else
      gqlApi rq
  $(logInfo) "finished handling graphql request"
  pure rs

api :: [Middleware] -> ScottyT LT.Text (AppM IO IO) ()
api ms = do
  middleware (cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))
  mapM middleware ms
  matchAny "/api" $ do
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw =<< (lift . gqlApiIO =<< body)
  get "/graphiql" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file "graphiql.html"
  get "/source/:id" $ do
    srcId <- param "id"
    case fromASCIIBytes srcId of
      Nothing -> status badRequest400
      Just uuid -> do
        lift (getSourceFilePath (SourceRef uuid)) >>= \case
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
        lift (findCoverImageIO (SourceRef uuid)) >>= \case
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
    orphans <- rescue (param "orphans") (const $ pure False)
    let filt = if orphans then API.Orphaned else API.AllSourceGroups
    rq <- body
    case fromASCIIBytes collectionId of
      Nothing -> status badRequest400
      Just uuid -> do
        appData <- lift ask
        stream $
          API.streamSourceGroupsQuery appData (CollectionRef uuid) mappingNames filt rq

data CoverImage = EmbeddedImage EmbeddedPicture | ExternalImageFile FilePath

findCoverImageIO :: SourceRef -> AppM IO IO (Maybe CoverImage)
findCoverImageIO k = withSpan "findCoverImageIO" defaultSpanArguments do
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
