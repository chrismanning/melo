module Melo.Library.Repo.Metadata where

import Control.Lens ((^.))
import Control.Monad
import Data.Functor
import Data.Hashable
import qualified Data.HashMap.Strict as H
import qualified Data.Map as Map
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Typeable
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Database.PostgreSQL.Simple (withTransaction)
import Haxl.Core
import Melo.API.Haxl
import Melo.Format.Internal.Metadata
import qualified Melo.Library.Database.Model as BM
import Network.URI
import System.Directory

import Debug.Trace

getMetadataSource :: BM.MetadataSourceKey -> Haxl (Maybe BM.MetadataSource)
getMetadataSource = dataFetch . GetMetadataSource

getFileMetadataSourceByPath :: FilePath -> Haxl [BM.MetadataSource]
getFileMetadataSourceByPath = dataFetch . GetFileMetadataSourceByPath

insertMetadataSource :: MetadataSource -> Haxl [BM.MetadataSourceKey]
insertMetadataSource = dataFetch . InsertMetadataSource

newtype MetadataSource = FileMetadataSource MetadataFile
  deriving (Eq, Show)

data MetadataSourceRepo a where
  GetMetadataSource :: BM.MetadataSourceKey -> MetadataSourceRepo (Maybe BM.MetadataSource)
  GetFileMetadataSourceByPath :: FilePath -> MetadataSourceRepo [BM.MetadataSource]
  InsertMetadataSource :: MetadataSource -> MetadataSourceRepo [BM.MetadataSourceKey]
  deriving (Typeable)

deriving instance Eq (MetadataSourceRepo a)

instance Hashable (MetadataSourceRepo a) where
  hashWithSalt s (GetMetadataSource _) = hashWithSalt s (0 :: Int)
  hashWithSalt s (GetFileMetadataSourceByPath _) = hashWithSalt s (1 :: Int)
  hashWithSalt s (InsertMetadataSource _) = hashWithSalt s (2 :: Int)

deriving instance Show (MetadataSourceRepo a)

instance ShowP MetadataSourceRepo where showp = show

instance StateKey MetadataSourceRepo where
  data State MetadataSourceRepo = MetadataSourceState {
    connPool :: Pool Connection
  } deriving (Generic)

instance DataSourceName MetadataSourceRepo where
  dataSourceName = const "MetadataSourceDataSource"

instance DataSource () MetadataSourceRepo where
  fetch state _flags _ = BackgroundFetch $ \blockedFetches -> do
    getMetadataSource' $ H.fromList [(k, r) | BlockedFetch (GetMetadataSource k) r <- blockedFetches]
    getFileMetadataSourcesByPath' $ Map.fromList [(fileUri p, r) | BlockedFetch (GetFileMetadataSourceByPath p) r <- blockedFetches]
    insertMetadataSource' []
    where
      getMetadataSource' :: H.HashMap BM.MetadataSourceKey (ResultVar (Maybe BM.MetadataSource)) -> IO ()
      getMetadataSource' vs = unless (null vs)
        $ withResource (state ^. #connPool)
        $ \conn -> do
          allMetadataSources <- runBeamPostgresDebug putStrLn conn $ runSelectReturningOne (select $ all_ $ BM.libraryDb ^. #metadata_source)
          mapM_ (`putSuccess` allMetadataSources) vs
      getFileMetadataSourcesByPath' :: Map.Map URI (ResultVar [BM.MetadataSource]) -> IO ()
      getFileMetadataSourcesByPath' vs = unless (null vs)
        $ withResource (state ^. #connPool)
        $ \conn -> do
          let srcs = Map.toList vs
          let q = filter_ (\m -> m ^. #source `in_` fmap (val_ . T.pack . show) (Map.keys vs)) (all_ $ BM.libraryDb ^. #metadata_source)
          allMetadataSources <- runBeamPostgresDebug putStrLn conn $ runSelectReturningList (select q)
          let msm = Map.fromListWith (++) $ fmap (\ms -> (fileUri $ T.unpack (ms ^. #source), [ms])) allMetadataSources
          forM_ srcs $ \(s, v) ->
            case Map.lookup s msm of
              Nothing -> pure ()
              Just ms -> putSuccess v ms
      insertMetadataSource' :: [(MetadataSource, ResultVar [BM.MetadataSourceKey])] -> IO ()
      insertMetadataSource' vs = unless (null vs)
        $ withResource (state ^. #connPool)
        $ \conn -> withTransaction conn $ do
          -- TODO put results in ResultVar
          runBeamPostgresDebug traceM conn $ Pg.runPgInsertReturningList $
            Pg.insertReturning
              (BM.libraryDb ^. #metadata_source)
              (insertExpressions (vs >>= metadataSources . fst))
              (Pg.onConflict
                  (B.conflictingFields (\t -> (t ^. #kind, t ^. #source)))
                  ( Pg.onConflictUpdateInstead
                      (^. #scanned)
                  )
              )
              (Just primaryKey)
          pure ()

getSource :: MetadataSource -> Text
getSource (FileMetadataSource f) = T.pack $ show (fileUri (f ^. #filePath))

getFileSource :: MetadataFile -> Text
getFileSource f = T.pack $ show (fileUri (f ^. #filePath))

modificationTime :: MetadataSource -> IO LocalTime
modificationTime (FileMetadataSource f) = utcToLocalTime utc <$> getModificationTime (f ^. #filePath)

metadataSources :: MetadataSource -> [BM.MetadataSourceT (QExpr Postgres s)]
metadataSources (FileMetadataSource f) = metadataFileSources f

metadataFileSources :: MetadataFile -> [BM.MetadataSourceT (QExpr Postgres s)]
metadataFileSources f =
  H.elems (f ^. #metadata) <&> \(Metadata (MetadataId fid) _ _ _) ->
    BM.MetadataSource
      { id = default_
      , kind = val_ fid
      , source = val_ $ getFileSource f
      , scanned = currentTimestamp_
      , idx = val_ Nothing
      }

fileUri :: FilePath -> URI
fileUri p =
  URI
    { uriScheme = "file:",
      uriAuthority = Nothing,
      uriPath = p,
      uriQuery = "",
      uriFragment = ""
    }
