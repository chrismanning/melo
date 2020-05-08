{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.Types
  ( NewImportSource (..),
    MetadataImportSource (..),
    NewSource (..),
    AudioRange (..),
    Source (..),
    ImportStats (..),
  )
where

import Basement.From
import Control.Lens hiding ((.=))
import Data.Aeson.Types as A
import Data.Coerce
import qualified Data.HashMap.Strict as H
import Data.Int
import Data.Maybe
import Data.Range as R
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.Beam
import Database.Beam.Postgres as Pg
import Melo.Common.Metadata
import Melo.Format.Info
import Melo.Format.Internal.Tag
import Melo.Format.Metadata
import qualified Melo.Library.Database.Model as DB
import Network.URI
import Numeric.Natural

data NewImportSource = FileSource MetadataFile | CueFileSource
  deriving (Eq, Show)

data MetadataImportSource = MetadataImportSource
  { metadata :: Metadata,
    audioInfo :: Info,
    src :: URI
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
          src
        }

getSourceUri :: NewImportSource -> URI
getSourceUri (FileSource f) = fileUri (f ^. #filePath)

getFileSourceUri :: MetadataFile -> URI
getFileSourceUri f = fileUri (f ^. #filePath)

getAllMetadata :: NewImportSource -> [Metadata]
getAllMetadata (FileSource f) = H.elems $ getFileMetadata f

getMetadata :: MetadataId -> NewImportSource -> Maybe Metadata
getMetadata mid (FileSource f) = H.lookup mid $ getFileMetadata f

getFileMetadata :: MetadataFile -> H.HashMap MetadataId Metadata
getFileMetadata f = f ^. #metadata

getInfo :: NewImportSource -> Info
getInfo (FileSource f) = getFileInfo f

getFileInfo :: MetadataFile -> Info
getFileInfo f = f ^. #audioInfo

data NewSource = NewSource
  { kind :: Text,
    metadataFormat :: Text,
    tags :: Tags,
    source :: Text,
    idx :: Maybe Int,
    range :: Maybe AudioRange
  }
  deriving (Show, Eq, Generic)

data AudioRange = SampleRange (Range Int64) | TimeRange (Range DB.Interval)
  deriving (Eq, Show)

instance From MetadataImportSource NewSource where
  from ms =
    NewSource
      { kind = "",
        metadataFormat = coerce (ms ^. #metadata . #formatId :: MetadataId),
        tags = ms ^. #metadata . #tags,
        source = T.pack $ show $ ms ^. #src,
        -- TODO multi-track files
        idx = Nothing,
        range = Nothing
      }

instance From NewSource (DB.SourceT (QExpr Postgres s)) where
  from s =
    DB.Source
      { id = default_,
        kind = val_ $ s ^. #kind,
        metadata_format = val_ $ s ^. #metadataFormat,
        metadata = val_ $ PgJSONB (tagsToValue $ s ^. #tags),
        source_uri = val_ $ s ^. #source,
        idx = val_ $ fromMaybe (-1 :: Int) (s ^. #idx),
        sample_range = val_ $ sampleRange =<< (s ^. #range),
        time_range = val_ $ timeRange =<< (s ^. #range),
        scanned = currentTimestamp_
      }

tagsToValue :: Tags -> A.Value
tagsToValue (Tags tags) = A.toJSON $
  tags <&> \(k, v) ->
    A.object
      [ "key" .= k,
        "value" .= v
      ]

valueToTags :: A.Value -> Maybe Tags
valueToTags (A.Array ts) = do
  tags <- V.forM ts $ \t ->
    flip A.parseMaybe t
      $ A.withObject "Tags"
      $ \obj -> do
        k <- obj .: "key"
        v <- obj .: "value"
        pure (k, v)
  pure (Tags tags)
valueToTags _ = Nothing

fileUri :: FilePath -> URI
fileUri p =
  URI
    { uriScheme = "file:",
      uriAuthority = Nothing,
      uriPath = escapeURIString (\c -> isUnreserved c || c == '/') p,
      uriQuery = "",
      uriFragment = ""
    }

sampleRange :: AudioRange -> Maybe (PgRange PgInt8Range Int64)
sampleRange (SampleRange range) = Just (toPgRange range)
sampleRange (TimeRange _) = Nothing

timeRange :: AudioRange -> Maybe (PgRange DB.IntervalRange DB.Interval)
timeRange (TimeRange range) = Just (toPgRange range)
timeRange (SampleRange _) = Nothing

toPgRange :: Range a -> PgRange b a
toPgRange (SingletonRange a) = PgRange (Pg.inclusive a) (Pg.inclusive a)
toPgRange (SpanRange a b) = PgRange (toPgRangeBound a) (toPgRangeBound b)
toPgRange (LowerBoundRange a) = PgRange (toPgRangeBound a) Pg.unbounded
toPgRange (UpperBoundRange a) = PgRange Pg.unbounded (toPgRangeBound a)
toPgRange InfiniteRange = PgRange Pg.unbounded Pg.unbounded

toPgRangeBound :: Bound a -> PgRangeBound a
toPgRangeBound (Bound a R.Inclusive) = Pg.inclusive a
toPgRangeBound (Bound a R.Exclusive) = Pg.exclusive a

data Source = Source
  { ref :: DB.SourceKey,
    metadata :: Metadata,
    source :: URI,
    range :: Maybe AudioRange
  }
  deriving (Generic)

instance TryFrom DB.Source Source where
  tryFrom s = do
    uri <- parseURI $ T.unpack (s ^. #source_uri)
    let mid = MetadataId $ s ^. #metadata_format
    tags <- valueToTags (coerce $ s ^. #metadata)
    metadata <- mkMetadata mid tags
    pure
      Source
        { ref = primaryKey s,
          range = Nothing,
          source = uri,
          metadata
        }

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
