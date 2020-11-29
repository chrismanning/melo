{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Melo.API where

import Control.Algebra
import Control.Carrier.Error.Church
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Lens ((^.))
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Lazy.Char8
import Data.Maybe
import Data.Morpheus
import Data.Morpheus.Types
import Data.Pool
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Typeable
import Data.UUID
import Database.Beam.Postgres (Connection)
import GHC.Generics
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Common.Metadata
import qualified Melo.Database.Model as DB
import Melo.Database.Transaction
import qualified Melo.Format.Error as F
import Melo.Library.API
import Melo.Library.Service
import Melo.Library.Source.Repo
import Network.HTTP.Types.Status
import Network.URI
import Network.Wai.Middleware.Cors
import Network.Wai.Parse (FileInfo (..))
import System.FilePath (takeFileName)
import Web.Scotty

data Query m = Query
  { library :: m (LibraryQuery m)
  }
  deriving (Generic, GQLType)

data Mutation m = Mutation
  { library :: m (LibraryMutation m)
  }
  deriving (Generic, GQLType)

rootResolver :: ResolverE sig m => RootResolver m () Query Mutation Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {library = resolveLibrary},
      mutationResolver = Mutation {library = resolveLibraryMutation},
      subscriptionResolver = Undefined
    }

gqlApi :: forall sig m. (ResolverE sig m, Typeable m) => ByteString -> m ByteString
gqlApi = interpreter (rootResolver @sig @m)

gqlApiIO :: Pool Connection -> ByteString -> IO ByteString
gqlApiIO pool r = runStdoutLogging $
  runReader pool $
    runTransaction $
      withTransaction $ \conn ->
        runResolverE conn $ do
          $(logDebug) $ "Handling GraphQL request: " <> r
          gqlApi r

type ResolverE sig m =
  ( Has (Lift IO) sig m,
    Has (Reader Connection) sig m,
    Has Transaction sig m,
    Has Savepoint sig m,
    Has Logging sig m,
    Has FileSystem sig m,
    Has SourceRepository sig m,
    Has (Error F.MetadataException) sig m,
    Has LibraryService sig m,
    Has MetadataService sig m
  )

runResolverE conn =
  runReader conn
    . runStdoutLogging
    . runError
      ( \(e :: F.MetadataException) -> do
          $(logError) ("Uncaught metadata error: " <> show e)
          error "unimplemented"
      )
      pure
    . runSavepoint
    . runMetadataServiceIO
    . runFileSystemIO
    . runSourceRepositoryIO
    . runLibraryServiceIO

api :: Pool Connection -> ScottyM ()
api pool = do
  middleware (cors (const $ Just simpleCorsResourcePolicy {corsRequestHeaders = ["Content-Type"]}))
  matchAny "/api" $ do
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw =<< (liftIO . gqlApiIO pool =<< body)
  get "/graphiql" $ do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file "graphiql.html"
  get "/source/:id" $ do
    srcId <- param "id"
    case fromASCIIBytes srcId of
      Nothing -> status badRequest400
      Just uuid -> do
        fileInfo <- liftIO $ streamSourceIO pool (DB.SourceKey uuid)
        case fileInfo of
          Nothing -> status notFound404
          Just fileInfo -> do
            setHeader "Content-Type" (decodeUtf8 $ LB.fromStrict $ fileContentType fileInfo)
            let fileName' = decodeUtf8 $ LB.fromStrict $ fileName fileInfo
            setHeader "Content-Disposition" ("attachment; filename=\"" <> fileName' <> "\"")
            raw (fileContent fileInfo)

streamSourceIO :: Pool Connection -> DB.SourceKey -> IO (Maybe (FileInfo LB.ByteString))
streamSourceIO pool k = withResource pool $ \conn ->
  runReader conn $
    runStdoutLogging $
      runFileSystemIO $
        runSourceRepositoryIO $ do
          s <- listToMaybe <$> getSources [k]
          case s >>= parseURI . T.unpack . (^. #source_uri) of
            Nothing -> pure Nothing
            Just uri -> do
              $(logDebug) $ "Source URI: " <> show uri
              case uriScheme uri of
                "file:" -> do
                  let path = unEscapeString $ uriPath uri
                  $(logInfo) $ "Opening file '" <> path <> "' for streaming"
                  x <- doesFileExist path
                  if x
                    then do
                      c <- sendIO $ LB.readFile path
                      pure $
                        Just
                          FileInfo
                            { fileName = encodeUtf8 $ T.pack $ takeFileName path,
                              fileContentType = "",
                              fileContent = c
                            }
                    else pure Nothing
                scheme -> do
                  $(logError) $ "Unsupported URI scheme " <> scheme
                  pure Nothing
