{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Metadata.Repo where

import Control.Algebra
import Control.Carrier.Reader
import Control.Lens ((^.))
import Data.Functor
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import GHC.Generics (Generic1)
import Melo.Format.Internal.Metadata
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query
import Network.URI
import System.Directory

data MetadataSourceRepository m k
  = GetMetadataSources [DB.MetadataSourceKey] ([DB.MetadataSource] -> m k)
  | GetMetadataSourcesBySrc [URI] ([DB.MetadataSource] -> m k)
  | InsertMetadataSources [NewMetadataSource] ([(DB.MetadataSourceKey, Text)] -> m k)
  | DeleteMetadataSources [DB.MetadataSourceKey] (m k)
  deriving (Functor, Generic1, HFunctor, Effect)

getMetadataSources :: Has MetadataSourceRepository sig m => [DB.MetadataSourceKey] -> m [DB.MetadataSource]
getMetadataSources ks = send (GetMetadataSources ks pure)

getMetadataSourcesBySrc :: Has MetadataSourceRepository sig m => [URI] -> m [DB.MetadataSource]
getMetadataSourcesBySrc srcs = send (GetMetadataSourcesBySrc srcs pure)

insertMetadataSources :: Has MetadataSourceRepository sig m => [NewMetadataSource] -> m [(DB.MetadataSourceKey, Text)]
insertMetadataSources ms = send (InsertMetadataSources ms pure)

deleteMetadataSources :: Has MetadataSourceRepository sig m => [DB.MetadataSourceKey] -> m ()
deleteMetadataSources ks = send (DeleteMetadataSources ks (pure ()))

newtype MetadataSourceRepositoryIOC m a
  = MetadataSourceRepositoryIOC
      { runMetadataSourceRepositoryIOC :: ReaderC Connection m a
      }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

instance
  (MonadIO m, Algebra sig m, Effect sig) =>
  Algebra (MetadataSourceRepository :+: sig) (MetadataSourceRepositoryIOC m)
  where
  alg = \case
    L (GetMetadataSources [] k) -> pure [] >>= k
    L (GetMetadataSources ks k) -> MetadataSourceRepositoryIOC $ do
      conn <- ask
      let ids = fmap (\(DB.MetadataSourceKey k') -> val_ k') ks
      let q = filter_ (\m -> m ^. #id `in_` ids) $ all_ (DB.libraryDb ^. #metadata_source)
      runMetadataSourceRepositoryIOC $ runPgDebug conn (runSelectReturningList (select q)) >>= k
    L (GetMetadataSourcesBySrc [] k) -> pure [] >>= k
    L (GetMetadataSourcesBySrc fs k) -> MetadataSourceRepositoryIOC $ do
      conn <- ask
      let q = filter_ (\m -> m ^. #source `in_` fmap (val_ . T.pack . show) fs) (all_ $ DB.libraryDb ^. #metadata_source)
      runMetadataSourceRepositoryIOC $ runPgDebug conn (runSelectReturningList (select q)) >>= k
    L (InsertMetadataSources [] k) -> pure [] >>= k
    L (InsertMetadataSources ms k) -> MetadataSourceRepositoryIOC $ do
      conn <- ask
      let q =
            Pg.insertReturning
              (DB.libraryDb ^. #metadata_source)
              (insertExpressions (ms >>= metadataSources))
              ( Pg.onConflict
                  (B.conflictingFields (\t -> (t ^. #kind, t ^. #source)))
                  ( Pg.onConflictUpdateInstead
                      (^. #scanned)
                  )
              )
              (Just (\m -> (primaryKey m, m ^. #source)))
      runMetadataSourceRepositoryIOC $ runPgDebug conn (Pg.runPgInsertReturningList q) >>= k
    L (DeleteMetadataSources [] k) -> k
    L (DeleteMetadataSources ks k) -> MetadataSourceRepositoryIOC $ do
      conn <- ask
      let q = delete (DB.libraryDb ^. #metadata_source) (\m -> m ^. #id `in_` fmap (\(DB.MetadataSourceKey k') -> val_ k') ks)
      runMetadataSourceRepositoryIOC $ runPgDebug conn (runDelete q) >> k
    R other -> MetadataSourceRepositoryIOC (alg (R (handleCoercible other)))

runMetadataSourceRepositoryIO :: Connection -> MetadataSourceRepositoryIOC m a -> m a
runMetadataSourceRepositoryIO conn = runReader conn . runMetadataSourceRepositoryIOC

newtype NewMetadataSource = FileMetadataSource MetadataFile
  deriving (Eq, Show)

getSource :: NewMetadataSource -> Text
getSource (FileMetadataSource f) = T.pack $ show (fileUri (f ^. #filePath))

getFileSource :: MetadataFile -> Text
getFileSource f = T.pack $ show (fileUri (f ^. #filePath))

modificationTime :: NewMetadataSource -> IO LocalTime
modificationTime (FileMetadataSource f) = utcToLocalTime utc <$> getModificationTime (f ^. #filePath)

metadataSources :: NewMetadataSource -> [DB.MetadataSourceT (QExpr Postgres s)]
metadataSources (FileMetadataSource f) = metadataFileSources f

metadataFileSources :: MetadataFile -> [DB.MetadataSourceT (QExpr Postgres s)]
metadataFileSources f =
  H.elems (f ^. #metadata) <&> \(Metadata (MetadataId fid) _ _ _) ->
    DB.MetadataSource
      { id = default_,
        kind = val_ fid,
        source = val_ $ getFileSource f,
        scanned = currentTimestamp_,
        idx = val_ Nothing
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