{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Service where

import Basement.From
import Control.Algebra
import Control.Applicative
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens hiding (from, lens)
import Control.Monad
import Data.Attoparsec.Text
import Data.Coerce
import Data.Foldable
import Data.Functor
--import Data.Generics.Labels ()
import qualified Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Database.Beam as B hiding (char, insert)
import Database.Beam.Backend.SQL.BeamExtensions as B
import Database.Beam.Postgres as Pg
import Database.Beam.Postgres.Full as Pg
import Melo.Common.Effect
import Melo.Common.Logging
import Melo.Common.Metadata
import Melo.Format.Info
import qualified Melo.Format.Mapping as M
import Melo.Format.Metadata
import Melo.Library.Album.Repo
import Melo.Library.Album.Service
import Melo.Library.Artist.Repo
import Melo.Library.Artist.Service
import qualified Melo.Library.Database.Model as DB
import Melo.Library.Database.Query
import qualified Melo.Library.Source.Repo as Repo
import Melo.Library.Source.Types
import Melo.Library.Track.Service
import Network.URI
import System.Directory

importSources :: Has SourceService sig m => [NewImportSource] -> m [Source]
importSources ss = send (ImportSources ss)

data SourceService :: Effect where
  ImportSources :: [NewImportSource] -> SourceService m [Source]
  UpdateSources :: [Source] -> SourceService m ()
  DeleteSources :: [Source] -> SourceService m ()
  SearchSourcesByUri :: Text -> SourceService m [Source]

newtype SourceServiceIOC m a = SourceServiceIOC
  { runSourceServiceIOC :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance
  ( Has Repo.SourceRepository sig m,
    Has AlbumService sig m,
    Has ArtistService sig m,
    Has TrackService sig m,
    Has Logging sig m
  ) =>
  Algebra (SourceService :+: sig) (SourceServiceIOC m)
  where
  alg _ (L sig) ctx = case sig of
    ImportSources ss -> do
      $(logDebug) $ "Importing sources: " <> show ss
      let metadataSources :: [MetadataImportSource] = mapMaybe tryFrom ss
      $(logDebug) $ "Importing metadata sources: " <> show metadataSources
      srcs <- Repo.insertSources (fmap from metadataSources)
      pure (ctx $> mapMaybe tryFrom srcs)
    UpdateSources ss -> do
      undefined
    DeleteSources ss -> do
      undefined
    SearchSourcesByUri t -> do
      undefined
  alg hdl (R other) ctx = SourceServiceIOC (alg (runSourceServiceIOC . hdl) other ctx)

length' :: (Foldable f, Num a) => f b -> a
length' = foldl' (const . (+ 1)) 0

modificationTime :: NewImportSource -> IO LocalTime
modificationTime (FileSource f) = utcToLocalTime utc <$> getModificationTime (f ^. #filePath)

runSourceServiceIO :: SourceServiceIOC m a -> m a
runSourceServiceIO = runSourceServiceIOC
