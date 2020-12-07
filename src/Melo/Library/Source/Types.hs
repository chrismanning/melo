{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Types
  ( NewImportSource (..),
    MetadataImportSource (..),
    NewSource (..),
    UpdateSource,
    AudioRange (..),
    SourceRef,
    Source (..),
    ImportStats (..),
  )
where

import Basement.From
import Control.Lens hiding ((.=))
import Data.Coerce
import qualified Data.HashMap.Strict as H
import Data.Int
import Data.Maybe
import Data.Range as R
import Data.Text (Text)
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Postgres as PgB
import Melo.Common.Metadata
import Melo.Common.Uri
import qualified Melo.Database.Model as DB
import Melo.Format.Info
import Melo.Format.Metadata
import Melo.Library.Collection.Types
import Network.URI hiding (escapeString)
import Numeric.Natural

data NewImportSource = FileSource CollectionRef MetadataFile | CueFileSource
  deriving (Eq, Show)

data MetadataImportSource = MetadataImportSource
  { metadata :: Metadata,
    audioInfo :: Info,
    src :: URI,
    metadataFileId :: MetadataFileId,
    collection :: CollectionRef
  }
  deriving (Show, Generic)

instance TryFrom NewImportSource MetadataImportSource where
  tryFrom s = do
    metadata <- chooseMetadata (getAllMetadata s)
    src <- parseURI (show $ getSourceUri s)
    pure
      MetadataImportSource
        { audioInfo = getInfo s,
          metadata,
          src,
          metadataFileId = getMetadataFileId s,
          collection = getCollectionRef s
        }

getSourceUri :: NewImportSource -> URI
getSourceUri (FileSource _ f) = fileUri (f ^. #filePath)

getFileSourceUri :: MetadataFile -> URI
getFileSourceUri f = fileUri (f ^. #filePath)

getAllMetadata :: NewImportSource -> [Metadata]
getAllMetadata (FileSource _ f) = H.elems $ getFileMetadata f

getMetadata :: MetadataId -> NewImportSource -> Maybe Metadata
getMetadata mid (FileSource _ f) = H.lookup mid $ getFileMetadata f

getFileMetadata :: MetadataFile -> H.HashMap MetadataId Metadata
getFileMetadata f = f ^. #metadata

getInfo :: NewImportSource -> Info
getInfo (FileSource _ f) = getFileInfo f

getFileInfo :: MetadataFile -> Info
getFileInfo f = f ^. #audioInfo

getMetadataFileId :: NewImportSource -> MetadataFileId
getMetadataFileId (FileSource _ f) = f ^. #fileId

getCollectionRef :: NewImportSource -> CollectionRef
getCollectionRef (FileSource ref _) = ref

data NewSource = NewSource
  { kind :: Text,
    metadataFormat :: Text,
    tags :: Tags,
    source :: Text,
    idx :: Maybe Int16,
    range :: Maybe AudioRange,
    collection :: CollectionRef
  }
  deriving (Show, Eq, Generic)

data AudioRange = SampleRange (Range Int64) | TimeRange (Range DB.Interval)
  deriving (Eq, Show)

instance From MetadataImportSource NewSource where
  from ms =
    NewSource
      { kind = ms ^. #metadataFileId . coerced,
        metadataFormat = coerce (ms ^. #metadata . #formatId :: MetadataId),
        tags = ms ^. #metadata . #tags,
        source = T.pack $ show $ ms ^. #src,
        -- TODO multi-track files
        idx = Nothing,
        range = Nothing,
        collection = ms ^. #collection
      }

instance From NewSource (DB.SourceT (QExpr Postgres s)) where
  from s =
    DB.Source
      { id = default_,
        kind = val_ $ s ^. #kind,
        metadata_format = val_ $ s ^. #metadataFormat,
        metadata = val_ $ s ^. #tags . coerced,
        source_uri = val_ $ s ^. #source,
        idx = val_ $ fromMaybe (-1 :: Int16) (s ^. #idx),
        sample_range = val_ $ sampleRange =<< (s ^. #range),
        time_range = val_ $ timeRange =<< (s ^. #range),
        scanned = currentTimestamp_,
        collection_id = val_ $ DB.CollectionKey (s ^. #collection . coerced)
      }

sampleRange :: AudioRange -> Maybe (PgRange PgInt8Range Int64)
sampleRange (SampleRange range) = Just (toPgRange range)
sampleRange (TimeRange _) = Nothing

timeRange :: AudioRange -> Maybe (PgRange DB.IntervalRange DB.Interval)
timeRange (TimeRange range) = Just (toPgRange range)
timeRange (SampleRange _) = Nothing

toPgRange :: Range a -> PgRange b a
toPgRange (SingletonRange a) = PgRange (PgB.inclusive a) (PgB.inclusive a)
toPgRange (SpanRange a b) = PgRange (toPgRangeBound a) (toPgRangeBound b)
toPgRange (LowerBoundRange a) = PgRange (toPgRangeBound a) PgB.unbounded
toPgRange (UpperBoundRange a) = PgRange PgB.unbounded (toPgRangeBound a)
toPgRange InfiniteRange = PgRange PgB.unbounded PgB.unbounded

toPgRangeBound :: Bound a -> PgRangeBound a
toPgRangeBound (Bound a R.Inclusive) = PgB.inclusive a
toPgRangeBound (Bound a R.Exclusive) = PgB.exclusive a

type SourceRef = DB.SourceKey

data Source = Source
  { ref :: SourceRef,
    metadata :: Metadata,
    source :: URI,
    range :: Maybe AudioRange
  }
  deriving (Generic, Show)

instance TryFrom DB.Source Source where
  tryFrom s = do
    uri <- parseURI $ T.unpack (s ^. #source_uri)
    let mid = MetadataId $ s ^. #metadata_format
    let PgJSONB (DB.SourceMetadata tags) = s ^. #metadata
    metadata <- mkMetadata mid (Tags tags)
    pure
      Source
        { ref = primaryKey s,
          range = Nothing,
          source = uri,
          metadata
        }

type UpdateSource = DB.Source

data ImportStats = ImportStats
  { sourcesImported :: Natural,
    tracksImported :: Natural,
    albumsImported :: Natural,
    artistsImported :: Natural,
    genresImported :: Natural
  }
  deriving (Eq, Show, Generic)

instance Semigroup ImportStats where
  a <> b =
    ImportStats
      { sourcesImported = a ^. #sourcesImported + b ^. #sourcesImported,
        tracksImported = a ^. #tracksImported + b ^. #tracksImported,
        albumsImported = a ^. #albumsImported + b ^. #albumsImported,
        artistsImported = a ^. #artistsImported + b ^. #artistsImported,
        genresImported = a ^. #genresImported + b ^. #genresImported
      }

instance Monoid ImportStats where
  mempty =
    ImportStats
      { sourcesImported = 0,
        tracksImported = 0,
        albumsImported = 0,
        artistsImported = 0,
        genresImported = 0
      }
