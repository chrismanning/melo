{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Melo.Library.Source.Types
  ( NewImportSource (..),
    MetadataImportSource (..),
    NewSource (..),
    UpdateSource,
    AudioRange (..),
    SourceRef (..),
    Source (..),
    ImportStats (..),
    SourceTable (..),
    SourceMetadata (..),
    SourceEntity,
    SourceMoveError(..),
    IntervalRange(..),
  )
where

import BinaryParser as BP
import Control.Applicative
import Control.Lens hiding ((.=), from)
import Control.Monad
import Data.Aeson hiding (Result)
import Data.ByteString (ByteString)
import Data.Char
import Data.Coerce
import Data.Either.Combinators
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
import Data.Time.LocalTime
import Data.UUID
import Data.UUID.V4
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word
import GHC.Generics hiding (from)
import qualified Hasql.Decoders as Hasql
import Melo.Common.Metadata
import Melo.Common.FileSystem
import Melo.Common.Uri
import Melo.Database.Repo (Entity (..))
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import Melo.Format.Metadata
import Melo.Library.Collection.Types
import Numeric.Natural
import Opaleye.Internal.HaskellDB.PrimQuery (PrimExpr(..), BoundExpr(..))
import qualified PostgreSQL.Binary.Decoding as BD
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

newtype IntervalRange = IntervalRange (Range CalendarDiffTime)
  deriving (Show, Eq)

instance DBType IntervalRange where
  typeInformation = TypeInformation {
    encode = encode',
    decode = decode',
    typeName = "intervalrange"
  }
    where
      encodeInterval :: CalendarDiffTime -> PrimExpr
      encodeInterval a = Rel8.encode (typeInformation @CalendarDiffTime) $ a
      encodeBound R.Bound{boundValue, boundType=R.Inclusive} = Inclusive (encodeInterval boundValue)
      encodeBound R.Bound{boundValue, boundType=R.Exclusive} = Exclusive (encodeInterval boundValue)
      encode' :: IntervalRange -> PrimExpr
      encode' (IntervalRange (SingletonRange a)) = RangeExpr "interval" (Inclusive (encodeInterval a)) (Inclusive (encodeInterval a))
      encode' (IntervalRange (SpanRange a b)) = RangeExpr "interval" (encodeBound a) (encodeBound b)
      encode' (IntervalRange (LowerBoundRange a)) = RangeExpr "interval" (encodeBound a) PosInfinity
      encode' (IntervalRange (UpperBoundRange a)) = RangeExpr "interval" NegInfinity (encodeBound a)
      encode' (IntervalRange InfiniteRange) = RangeExpr "interval" NegInfinity PosInfinity
      decode' :: Hasql.Value IntervalRange
      decode' = Hasql.custom decodeRange
      decodeRange :: Bool -> ByteString -> Either Text IntervalRange
      decodeRange integerDatetimes = BP.run (rangeParser integerDatetimes)
      rangeParser :: Bool -> BP.BinaryParser IntervalRange
      rangeParser integerDatetimes = IntervalRange <$>
            (spanRangeParser integerDatetimes
        <|> lowerBoundRangeParser integerDatetimes
        <|> upperBoundRangeParser integerDatetimes
        <|> infiniteRangeParser)
      lowerBoundRangeParser :: Bool -> BP.BinaryParser (Range CalendarDiffTime)
      lowerBoundRangeParser integerDatetimes = do
        lower <- lowerRangeBoundParser integerDatetimes
        BP.matchingByte commaUnit
        void upperRangeBoundTypeParser
        pure $ LowerBoundRange lower
      upperBoundRangeParser :: Bool -> BP.BinaryParser (Range CalendarDiffTime)
      upperBoundRangeParser integerDatetimes = do
        void lowerRangeBoundTypeParser
        BP.matchingByte commaUnit
        upper <- upperRangeBoundParser integerDatetimes
        pure $ UpperBoundRange upper
      infiniteRangeParser :: BP.BinaryParser (Range CalendarDiffTime)
      infiniteRangeParser = do
        void lowerRangeBoundTypeParser
        BP.matchingByte commaUnit
        void upperRangeBoundTypeParser
        pure InfiniteRange
      spanRangeParser :: Bool -> BP.BinaryParser (Range CalendarDiffTime)
      spanRangeParser integerDatetimes = do
        lower <- lowerRangeBoundParser integerDatetimes
        BP.matchingByte commaUnit
        upper <- upperRangeBoundParser integerDatetimes
        pure $ SpanRange lower upper
      commaUnit :: Word8 -> Either Text ()
      commaUnit c | c == fromIntegral (ord ',') = Right ()
      commaUnit _ = Left $ T.pack "Expected comma range separator"
      lowerRangeBoundParser :: Bool -> BP.BinaryParser (R.Bound CalendarDiffTime)
      lowerRangeBoundParser integerDatetimes = do
        t <- lowerRangeBoundTypeParser
        R.Bound <$> intervalParser integerDatetimes <*> pure t
      upperRangeBoundParser :: Bool -> BP.BinaryParser (R.Bound CalendarDiffTime)
      upperRangeBoundParser integerDatetimes =
        R.Bound <$> intervalParser integerDatetimes <*> upperRangeBoundTypeParser
      lowerRangeBoundTypeParser :: BP.BinaryParser R.BoundType
      lowerRangeBoundTypeParser = BP.matchingByte $ \case
        b | b == fromIntegral (ord '[') -> Right R.Inclusive
        b | b == fromIntegral (ord '(') -> Right R.Exclusive
        _ -> Left (T.pack "Expected lower range bound")
      upperRangeBoundTypeParser :: BP.BinaryParser R.BoundType
      upperRangeBoundTypeParser = BP.matchingByte $ \case
        b | b == fromIntegral (ord ']') -> Right R.Inclusive
        b | b == fromIntegral (ord ')') -> Right R.Exclusive
        _ -> Left (T.pack "Expected upper range bound")
      intervalParser :: Bool -> BP.BinaryParser CalendarDiffTime
      intervalParser True = CalendarDiffTime 0 . realToFrac <$> BD.interval_int
      intervalParser False = CalendarDiffTime 0 . realToFrac <$> BD.interval_float

instance From NewSource (SourceTable Expr) where
  from s =
    SourceTable
      { id = nullaryFunction "uuid_generate_v4",
        kind = lit $ s ^. #kind,
        metadata_format = lit $ s ^. #metadataFormat,
        metadata = lit $ s ^. #tags . coerced,
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
        metadata = s ^. #tags . coerced,
        source_uri = s ^. #source,
        idx = fromMaybe (-1 :: Int16) (s ^. #idx),
        time_range = rightToMaybe =<< tryFrom <$> s ^. #range,
        --        sample_range = val_ $ sampleRange =<< (s ^. #range),
        scanned = unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = s ^. #collection . coerced
      }

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime

newtype SourceMetadata = SourceMetadata (Vector (Text, Text))
  deriving (Show, Eq, Generic)
  deriving (DBType) via JSONBEncoded SourceMetadata

instance From Tags SourceMetadata where
  from (Tags tags) = SourceMetadata tags

instance From SourceMetadata Tags where
  from (SourceMetadata tags) = Tags tags

instance ToJSON SourceMetadata where
  toJSON (SourceMetadata tags) =
    object
      [ "tags"
          .= toJSON
            ( tags <&> \(k, v) ->
                object
                  [ "key" .= k,
                    "value" .= v
                  ]
            )
      ]

instance FromJSON SourceMetadata where
  parseJSON = withObject "SourceMetadata" $
    \sm -> do
      tagPairs <- sm .: "tags"
      withArray
        "Tags"
        ( \arr ->
            do
              tags <- V.forM arr $
                withObject "Tag" $
                  \obj ->
                    do
                      k <- obj .: "key"
                      v <- obj .: "value"
                      pure (k, v)
              pure (SourceMetadata tags)
        )
        tagPairs

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
  tryFrom s = maybeToRight (TryFromException s Nothing) $ do
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

data AudioRange = SampleRange (Range Int64) | TimeRange (Range CalendarDiffTime)
  deriving (Eq, Show)

instance TryFrom AudioRange IntervalRange where
  tryFrom sr@SampleRange{} = Left $ TryFromException sr Nothing 
  tryFrom (TimeRange r) = Right $ IntervalRange r

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
    range :: Maybe AudioRange,
    collectionRef :: CollectionRef
  }
  deriving (Generic, Show, Eq)

instance TryFrom SourceEntity Source where
  tryFrom s = maybeToRight (TryFromException s Nothing) $ do
    uri <- parseURI $ T.unpack (s ^. #source_uri)
    let mid = MetadataId $ s ^. #metadata_format
    let JSONBEncoded (SourceMetadata tags) = s ^. #metadata
    metadata <- mkMetadata mid (Tags tags)
    pure
      Source
        { ref = s ^. #id,
          range = Nothing,
          source = uri,
          kind = MetadataFileId (s ^. #kind),
          collectionRef = CollectionRef $ s ^. #collection_id,
          metadata
        }

instance From Source SourceEntity where
  -- TODO
  from Source{metadata=Metadata{..},..} = SourceTable {
      id = ref,
      kind = coerce kind,
      metadata_format = coerce formatId,
      metadata = JSONBEncoded (from tags),
      source_uri = T.pack $ show source,
      -- TODO multi-track source
      idx = 0,
      time_range = Nothing,
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
