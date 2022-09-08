{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Source.Types where

import BinaryParser as BP
import Control.Applicative
import Control.Lens hiding ((.=), from)
import Data.Aeson hiding (Result)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Coerce
import Data.Either.Combinators
import Data.Fixed
import Data.Hashable
import qualified Data.HashMap.Strict as H
import Data.Int
import Data.Maybe
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Range (Range(..))
import qualified Data.Range as R
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Data.UUID
import Data.UUID.V4
import qualified Data.Vector as V
import Data.Vector (Vector())
import GHC.Generics hiding (from)
import qualified Hasql.Decoders as Hasql
import Melo.Common.Metadata
import Melo.Common.FileSystem
import Melo.Common.Uri
import Melo.Database.Repo (Entity (..))
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import qualified Melo.Format.Mapping as Mapping
import Melo.Format.Metadata
import Melo.Library.Collection.Types
import Numeric.Natural
import Opaleye.Internal.HaskellDB.PrimQuery (PrimExpr(..), BoundExpr(..), Literal(..))
import Rel8
  ( Column,
    DBEq,
    DBType (..),
    Expr,
    JSONBEncoded (..),
    Rel8able,
    Result,
    TypeInformation(..),
    lit,
    nullaryFunction,
  )
import System.IO.Unsafe
import Witch

data SourceTable f = SourceTable
  { id :: Column f SourceRef,
    kind :: Column f Text,
    metadata_format :: Column f Text,
    metadata :: Column f (JSONBEncoded SourceMetadata),
    source_uri :: Column f Text,
    idx :: Column f Int16,
    time_range :: Column f (Maybe IntervalRange),
    --  sample_range :: Column f (Maybe (PgRange PgInt8Range Int64)),
    scanned :: Column f LocalTime,
    collection_id :: Column f UUID
  }
  deriving (Generic, Rel8able)

type SourceEntity = SourceTable Result

deriving newtype instance Show (JSONBEncoded SourceMetadata)

deriving instance Show SourceEntity

instance Entity SourceEntity where
  type NewEntity SourceEntity = NewSource
  type PrimaryKey SourceEntity = SourceRef
  primaryKey e = e.id

newtype IntervalRange = IntervalRange (Range CalendarDiffTime)
  deriving (Show, Eq)

instance DBType IntervalRange where
  typeInformation = TypeInformation {
    encode = encode',
    decode = decode',
    typeName = "intervalrange"
  }
    where
      encode' :: IntervalRange -> PrimExpr
      encode' (IntervalRange (SingletonRange a)) = RangeExpr "intervalrange" (Inclusive (encodeInterval a)) (Inclusive (encodeInterval a))
      encode' (IntervalRange (SpanRange a b)) = RangeExpr "intervalrange" (encodeBound a) (encodeBound b)
      encode' (IntervalRange (LowerBoundRange a)) = RangeExpr "intervalrange" (encodeBound a) PosInfinity
      encode' (IntervalRange (UpperBoundRange a)) = RangeExpr "intervalrange" NegInfinity (encodeBound a)
      encode' (IntervalRange InfiniteRange) = RangeExpr "intervalrange" NegInfinity PosInfinity
      encodeInterval :: CalendarDiffTime -> PrimExpr
      encodeInterval = CastExpr "interval" . ConstExpr . StringLit . formatShow (alternativeDurationTimeFormat BasicFormat)
      encodeBound R.Bound{boundValue, boundType=R.Inclusive} = Inclusive (encodeInterval boundValue)
      encodeBound R.Bound{boundValue, boundType=R.Exclusive} = Exclusive (encodeInterval boundValue)
      decode' :: Hasql.Value IntervalRange
      decode' = Hasql.custom decodeRange
      decodeRange :: Bool -> ByteString -> Either Text IntervalRange
      decodeRange integerDatetimes = BP.run (rangeParser integerDatetimes)
      rangeParser :: Bool -> BP.BinaryParser IntervalRange
      rangeParser integerDatetimes = IntervalRange <$> do
        flags <- BP.byte
        let isEmpty = 0 /= flags .&. 0x1
        let hasLower = 0 == (flags .&. (0x01 .|. 0x20 .|. 0x08))
        let hasUpper = 0 == (flags .&. (0x01 .|. 0x40 .|. 0x10))
        if isEmpty then pure $ R.SpanRange (R.Bound (CalendarDiffTime 0 0) R.Exclusive) (R.Bound (CalendarDiffTime 0 0) R.Exclusive)
        else do
          if hasLower then do
            lower <- intervalParser integerDatetimes
            let lowerBound = R.Bound lower (if 0 /= (flags .&. 0x02) then R.Inclusive else R.Exclusive)
            if hasUpper then do
              upper <- intervalParser integerDatetimes
              let upperBound = R.Bound upper (if 0 /= (flags .&. 0x04) then R.Inclusive else R.Exclusive)
              pure $ R.SpanRange lowerBound upperBound
            else
              pure $ R.LowerBoundRange lowerBound
          else if hasUpper then do
            upper <- intervalParser integerDatetimes
            let upperBound = R.Bound upper (if 0 /= (flags .&. 0x04) then R.Inclusive else R.Exclusive)
            pure $ R.UpperBoundRange upperBound
          else pure R.InfiniteRange
      intervalParser :: Bool -> BP.BinaryParser CalendarDiffTime
      intervalParser True = do
        _size <- BP.bytesOfSize 4
        time <- intSized @Int64 8
        _days <- intSized @Int32 4
        _months <- intSized @Int32 4
        let microseconds :: Micro = MkFixed (toInteger time)
        pure $ CalendarDiffTime 0 (realToFrac microseconds)
      intervalParser False = BP.failure "floating point datetimes not supported"
      packNum :: (Bits a, Num a) => ByteString -> a
      packNum =
        B.foldl' (\n h -> shiftL n 8 .|. fromIntegral h) 0
      intSized :: (Bits a, Integral a) => Int -> BinaryParser a
      intSized n = packNum <$> bytesOfSize n

instance From NewSource (SourceTable Expr) where
  from s =
    SourceTable
      { id = nullaryFunction "uuid_generate_v4",
        kind = lit $ s ^. #kind,
        metadata_format = lit $ s ^. #metadataFormat,
        metadata = lit $ JSONBEncoded $ SourceMetadata (uncurry TagPair `V.map` coerce s.tags),
        source_uri = lit $ s ^. #source,
        idx = lit $ fromMaybe (-1 :: Int16) (s ^. #idx),
        --        sample_range = val_ $ sampleRange =<< (s ^. #range),
        time_range = lit $ rightToMaybe =<< tryFrom <$> s ^. #range,
        --        time_range = val_ $ timeRange =<< (s ^. #range),
        scanned = lit $ unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = lit $ s ^. #collection . coerced
      }

instance From NewSource SourceEntity where
  from s =
    SourceTable
      { id = SourceRef $ unsafeDupablePerformIO nextRandom,
        kind = s ^. #kind,
        metadata_format = s ^. #metadataFormat,
        metadata = JSONBEncoded $ SourceMetadata (uncurry TagPair `V.map` coerce s.tags),
        source_uri = s ^. #source,
        idx = fromMaybe (-1 :: Int16) (s ^. #idx),
        time_range = rightToMaybe =<< tryFrom <$> s ^. #range,
        --        sample_range = val_ $ sampleRange =<< (s ^. #range),
        scanned = unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = s ^. #collection . coerced
      }

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime

newtype SourceMetadata = SourceMetadata { tags :: Vector TagPair }
  deriving (Show, Eq, Generic)
  deriving (DBType) via JSONBEncoded SourceMetadata

instance From Tags SourceMetadata where
  from (Tags tags) = SourceMetadata (uncurry TagPair <$> tags)

instance From SourceMetadata Tags where
  from (SourceMetadata tags) = Tags (tags <&> (\p -> (p.key, p.value)))

data TagPair = TagPair {
  key :: Text,
  value :: Text
}
  deriving (Show, Eq, Generic, GQLType)

instance ToJSON TagPair where
  toJSON t = object ["key" .= t.key, "value" .= t.value]
  toEncoding t = pairs ("key" .= t.key <> "value" .= t.value)

deriving instance FromJSON TagPair

instance ToJSON SourceMetadata where
  toJSON (SourceMetadata tags) =
    object ["tags" .= tags]
  toEncoding (SourceMetadata tags) =
    pairs ("tags" .= tags)

deriving anyclass instance FromJSON SourceMetadata

data NewImportSource = FileSource CollectionRef MetadataFile
  | CueFileImportSource CollectionRef CueFileSource
  deriving (Eq, Show)

data CueFileSource = CueFileSource
  { metadata :: Metadata,
    idx :: Int16,
    audioInfo :: Info,
    range :: AudioRange,
    fileId :: MetadataFileId,
    filePath :: FilePath,
    cueFilePath :: FilePath
  }
  deriving (Eq, Show, Generic)

data MetadataImportSource = MetadataImportSource
  { metadata :: Metadata,
    audioInfo :: Info,
    src :: URI,
    metadataFileId :: MetadataFileId,
    idx :: Maybe Int16,
    range :: Maybe AudioRange,
    collection :: CollectionRef
  }
  deriving (Show, Generic)

instance TryFrom NewImportSource MetadataImportSource where
  tryFrom s = maybeToRight (TryFromException s Nothing) $ do
    metadata <- chooseMetadata (getAllMetadata s)
    src <- parseURI (show $ getSourceUri s)
    pure
      MetadataImportSource
        { audioInfo = getInfo s,
          metadata,
          src,
          metadataFileId = getMetadataFileId s,
          idx = getIdx s,
          range = getRange s,
          collection = getCollectionRef s
        }
    where
      getIdx (CueFileImportSource _ CueFileSource{idx}) = Just idx
      getIdx _ = Nothing
      getRange (CueFileImportSource _ CueFileSource{range}) = Just range
      getRange _ = Nothing

getSourceUri :: NewImportSource -> URI
getSourceUri (FileSource _ f) = fileUri (f ^. #filePath)
getSourceUri (CueFileImportSource _ CueFileSource{..}) = fileUri filePath

getFileSourceUri :: MetadataFile -> URI
getFileSourceUri f = fileUri (f ^. #filePath)

getAllMetadata :: NewImportSource -> [Metadata]
getAllMetadata (FileSource _ f) = H.elems $ getFileMetadata f
getAllMetadata (CueFileImportSource _ CueFileSource{..}) = [metadata]

getMetadata :: MetadataId -> NewImportSource -> Maybe Metadata
getMetadata mid (FileSource _ f) = H.lookup mid $ getFileMetadata f
getMetadata (MetadataId "CUE") (CueFileImportSource _ CueFileSource{..}) = Just metadata
getMetadata _ (CueFileImportSource _ CueFileSource{}) = Nothing

getFileMetadata :: MetadataFile -> H.HashMap MetadataId Metadata
getFileMetadata f = f ^. #metadata

getInfo :: NewImportSource -> Info
getInfo (FileSource _ f) = getFileInfo f
getInfo (CueFileImportSource _ CueFileSource{..}) = audioInfo

getFileInfo :: MetadataFile -> Info
getFileInfo f = f ^. #audioInfo

getMetadataFileId :: NewImportSource -> MetadataFileId
getMetadataFileId (FileSource _ f) = f ^. #fileId
getMetadataFileId (CueFileImportSource _ CueFileSource{..}) = fileId

getCollectionRef :: NewImportSource -> CollectionRef
getCollectionRef (FileSource ref _) = ref
getCollectionRef (CueFileImportSource ref _) = ref

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

data AudioRange = SampleRange (Range Int64) | TimeRange (Range CalendarDiffTime)
  deriving (Eq, Show)

instance TryFrom AudioRange IntervalRange where
  tryFrom sr@SampleRange{} = Left $ TryFromException sr Nothing
  tryFrom (TimeRange r) = Right $ IntervalRange r

instance From IntervalRange AudioRange where
  from (IntervalRange r) = TimeRange r

instance From MetadataImportSource NewSource where
  from ms =
    NewSource
      { kind = coerce ms.metadataFileId,
        metadataFormat = coerce ms.metadata.formatId,
        tags = ms.metadata.tags,
        source = T.pack $ show $ ms.src,
        idx = ms.idx,
        range = ms.range,
        collection = ms.collection
      }

--sampleRange :: AudioRange -> Maybe (PgRange PgInt8Range Int64)
--sampleRange (SampleRange range) = Just (toPgRange range)
--sampleRange (TimeRange _) = Nothing
--
--timeRange :: AudioRange -> Maybe (PgRange DB.IntervalRange DB.Interval)
--timeRange (TimeRange range) = Just (toPgRange range)
--timeRange (SampleRange _) = Nothing
--
--toPgRange :: Range a -> PgRange b a
--toPgRange (SingletonRange a) = PgRange (PgB.inclusive a) (PgB.inclusive a)
--toPgRange (SpanRange a b) = PgRange (toPgRangeBound a) (toPgRangeBound b)
--toPgRange (LowerBoundRange a) = PgRange (toPgRangeBound a) PgB.unbounded
--toPgRange (UpperBoundRange a) = PgRange PgB.unbounded (toPgRangeBound a)
--toPgRange InfiniteRange = PgRange PgB.unbounded PgB.unbounded
--
--toPgRangeBound :: Bound a -> PgRangeBound a
--toPgRangeBound (Bound a R.Inclusive) = PgB.inclusive a
--toPgRangeBound (Bound a R.Exclusive) = PgB.exclusive a

newtype SourceRef = SourceRef { unSourceRef :: UUID }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)

instance GQLType SourceRef where
  type KIND SourceRef = SCALAR

instance EncodeScalar SourceRef where
  encodeScalar (SourceRef uuid) = M.String $ toText uuid

instance DecodeScalar SourceRef where
  decodeScalar (M.String s) = case fromText s of
    Nothing -> Left "SourceRef must be UUID"
    Just uuid -> Right $ SourceRef uuid
  decodeScalar _ = Left "SourceRef must be a String"

instance From SourceRef UUID where
  from (SourceRef uuid) = uuid

data Source = Source
  { ref :: SourceRef,
    metadata :: Metadata,
    source :: URI,
    kind :: MetadataFileId,
    multiTrack :: Maybe MultiTrackDesc,
    collectionRef :: CollectionRef
  }
  deriving (Generic, Show, Eq)

instance TryFrom SourceEntity Source where
  tryFrom s = maybeToRight (TryFromException s Nothing) $ do
    uri <- parseURI $ T.unpack (s ^. #source_uri)
    let mid = MetadataId $ s ^. #metadata_format
    let JSONBEncoded (SourceMetadata tags) = s ^. #metadata
    let tags' = Tags $ tags <&> (\p -> (p.key, p.value))
    metadata <- mkCueMetadata mid tags' <|> mkMetadata mid tags'
    let multiTrack = case (s ^. #idx, s ^. #time_range) of
          (idx, Just timeRange) | idx > -1 -> Just MultiTrackDesc {idx, range = from timeRange}
          _ -> Nothing
    pure
      Source
        { ref = s ^. #id,
          multiTrack,
          source = uri,
          kind = MetadataFileId (s ^. #kind),
          collectionRef = CollectionRef $ s ^. #collection_id,
          metadata
        }

mkCueMetadata :: MetadataId -> Tags -> Maybe Metadata
mkCueMetadata (MetadataId mid) tags | mid == "CUE" = Just $ Metadata {
    tags,
    formatId = MetadataId mid,
    formatDesc = "CUE file",
    lens = mappedTag Mapping.cue
  }
mkCueMetadata _ _ = Nothing

data MultiTrackDesc = MultiTrackDesc {
  idx :: Int16,
  range :: AudioRange
} deriving (Generic, Show, Eq)

instance From Source SourceEntity where
  from Source{metadata=Metadata{..},..} = SourceTable {
      id = ref,
      kind = coerce kind,
      metadata_format = coerce formatId,
      metadata = JSONBEncoded (from tags),
      source_uri = T.pack $ show source,
      idx = fromMaybe (-1) (multiTrack ^? _Just . #idx),
      time_range = rightToMaybe . tryFrom =<< multiTrack ^? _Just . #range,
      --  sample_range :: Column f (Maybe (PgRange PgInt8Range Int64)),
      scanned = unsafeDupablePerformIO getCurrentLocalTime,
      collection_id = coerce collectionRef
    }

type UpdateSource = SourceEntity

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

data SourceMoveError
  = FileSystemMoveError MoveError
  | PatternError
  | NoSuchSource
  | SourcePathError
  | SourceUpdateError
  | WrongCollection
  | ConversionError (TryFromException SourceEntity Source)
  deriving (Show)

instance From MoveError SourceMoveError where
  from = FileSystemMoveError
