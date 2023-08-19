{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Source.Types where

import BinaryParser as BP
import Control.Applicative
import Control.Lens (to)
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Coerce
import Data.Either.Combinators
import Data.Fixed
import Data.HashMap.Strict qualified as H
import Data.Hashable
import Data.Int
import Data.Morpheus.Kind
import Data.Morpheus.Types as M
import Data.Range (Range (..))
import Data.Range qualified as R
import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime ())
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Data.UUID
import Data.UUID.V4
import Data.Vector qualified as V
import GHC.Generics hiding (from, to)
import Hasql.Decoders qualified as Hasql
import Melo.Common.FileSystem
import Melo.Common.Uri
import Melo.Database.Repo (Entity (..))
import Melo.Format.Info
import Melo.Format.Internal.Metadata
import Melo.Format.Mapping qualified as Mapping
import Melo.Format.Metadata
import Melo.Library.Collection.Types
import Numeric.Natural
import Opaleye.Internal.HaskellDB.PrimQuery (BoundExpr (..), Literal (..), PrimExpr (..))
import Rel8
  ( Column,
    DBEq,
    DBType (..),
    Expr,
    JSONBEncoded (..),
    ReadShow(..),
    Rel8able,
    Result,
    TypeInformation (..),
    lit,
    nullaryFunction,
  )
import System.IO.Unsafe
import Text.Read
import Prelude hiding ((.=))

-- TODO source attributes/flags - no linked artist, album, track - missing certain tags

data SourceTable f = SourceTable
  { id :: Column f SourceRef,
    kind :: Column f Text,
    metadata_format :: Column f Text,
    metadata :: Column f (JSONBEncoded SourceMetadata),
    source_uri :: Column f Text,
    idx :: Column f Int16,
    time_range :: Column f (Maybe IntervalRange),
    scanned :: Column f LocalTime,
    collection_id :: Column f CollectionRef,
    cover :: Column f (Maybe PictureTypeWrapper)
  }
  deriving (Generic, Rel8able)

type SourceEntity = SourceTable Result

deriving instance Show SourceEntity
deriving via (FromGeneric SourceEntity) instance TextShow SourceEntity

instance Entity SourceEntity where
  type NewEntity SourceEntity = NewSource
  type PrimaryKey SourceEntity = SourceRef
  primaryKey e = e.id

newtype PictureTypeWrapper = PictureTypeWrapper PictureType
  deriving (Generic, Show, Eq)
  deriving DBType via ReadShow PictureType
  deriving TextShow via FromGeneric PictureTypeWrapper

instance GQLType PictureTypeWrapper where
  type KIND PictureTypeWrapper = SCALAR

instance EncodeScalar PictureTypeWrapper where
  encodeScalar (PictureTypeWrapper pictureType) =
    M.String $ showt pictureType

instance DecodeScalar PictureTypeWrapper where
  decodeScalar (M.String s) =
    first T.pack $ PictureTypeWrapper <$> readEither (T.unpack s)
  decodeScalar _ = Left "PictureType must be a String"

newtype IntervalRange = IntervalRange (Range CalendarDiffTime)
  deriving (Show, Eq)
  deriving TextShow via FromStringShow IntervalRange

rangeLength :: IntervalRange -> Maybe NominalDiffTime
rangeLength (IntervalRange r) = rangeLength' r
  where
    rangeLength' (SpanRange lower upper) = Just $ upper.boundValue.ctTime - lower.boundValue.ctTime
    rangeLength' (UpperBoundRange upper) = Just upper.boundValue.ctTime
    rangeLength' _ = Nothing

instance DBType IntervalRange where
  typeInformation =
    TypeInformation
      { encode = encode',
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
      encodeBound R.Bound {boundValue, boundType = R.Inclusive} = Inclusive (encodeInterval boundValue)
      encodeBound R.Bound {boundValue, boundType = R.Exclusive} = Exclusive (encodeInterval boundValue)
      decode' :: Hasql.Value IntervalRange
      decode' = Hasql.custom decodeRange
      decodeRange :: Bool -> ByteString -> Either Text IntervalRange
      decodeRange integerDatetimes = BP.run (rangeParser integerDatetimes)
      rangeParser :: Bool -> BP.BinaryParser IntervalRange
      rangeParser integerDatetimes =
        IntervalRange <$> do
          flags <- BP.byte
          let isEmpty = 0 /= flags .&. 0x1
          let hasLower = 0 == (flags .&. (0x01 .|. 0x20 .|. 0x08))
          let hasUpper = 0 == (flags .&. (0x01 .|. 0x40 .|. 0x10))
          if isEmpty
            then pure $ R.SpanRange (R.Bound (CalendarDiffTime 0 0) R.Exclusive) (R.Bound (CalendarDiffTime 0 0) R.Exclusive)
            else do
              if hasLower
                then do
                  lower <- intervalParser integerDatetimes
                  let lowerBound = R.Bound lower (if 0 /= (flags .&. 0x02) then R.Inclusive else R.Exclusive)
                  if hasUpper
                    then do
                      upper <- intervalParser integerDatetimes
                      let upperBound = R.Bound upper (if 0 /= (flags .&. 0x04) then R.Inclusive else R.Exclusive)
                      pure $ R.SpanRange lowerBound upperBound
                    else pure $ R.LowerBoundRange lowerBound
                else
                  if hasUpper
                    then do
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
        kind = lit s.kind,
        metadata_format = lit s.metadataFormat,
        metadata = lit $ JSONBEncoded $ SourceMetadata (uncurry TagPair `V.map` coerce s.tags),
        source_uri = lit s.source,
        idx = lit $ fromMaybe (-1 :: Int16) s.idx,
        time_range = lit $ from <$> s.range,
        scanned = lit $ unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = lit $ coerce s.collection,
        cover = lit $ coerce s.cover
      }

-- instance used for preview transforms only
instance From NewSource SourceEntity where
  from s =
    SourceTable
      { id = SourceRef $ unsafeDupablePerformIO nextRandom,
        kind = s.kind,
        metadata_format = s.metadataFormat,
        metadata = JSONBEncoded $ SourceMetadata (uncurry TagPair `V.map` coerce s.tags),
        source_uri = s.source,
        idx = fromMaybe (-1 :: Int16) s.idx,
        time_range = from <$> s.range,
        scanned = unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = coerce s.collection,
        cover = coerce s.cover
      }

getCurrentLocalTime :: IO LocalTime
getCurrentLocalTime = zonedTimeToLocalTime <$> getZonedTime

newtype SourceMetadata = SourceMetadata {tags :: Vector TagPair}
  deriving (Show, Eq, Generic)
  deriving (DBType) via JSONBEncoded SourceMetadata
  deriving TextShow via FromGeneric SourceMetadata

instance From Tags SourceMetadata where
  from (Tags tags) = SourceMetadata (uncurry TagPair <$> tags)

instance From SourceMetadata Tags where
  from (SourceMetadata tags) = Tags (tags <&> (\p -> (p.key, p.value)))

data TagPair = TagPair
  { key :: Text,
    value :: Text
  }
  deriving (Show, Eq, Generic, GQLType)
  deriving TextShow via FromGeneric TagPair

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

data NewImportSource
  = FileSource CollectionRef MetadataFile
  | CueFileImportSource CollectionRef CueFileSource
  deriving (Eq, Show)
  deriving TextShow via FromStringShow NewImportSource

data CueFileSource = CueFileSource
  { metadata :: Metadata,
    idx :: Int16,
    audioInfo :: Info,
    range :: AudioRange,
    fileId :: MetadataFileId,
    filePath :: FilePath,
    cueFilePath :: FilePath,
    pictures :: [(PictureType, EmbeddedPicture)]
  }
  deriving (Eq, Show, Generic)
  deriving TextShow via FromGeneric CueFileSource

data MetadataImportSource = MetadataImportSource
  { metadata :: Maybe Metadata,
    audioInfo :: Info,
    src :: URI,
    metadataFileId :: MetadataFileId,
    idx :: Maybe Int16,
    range :: Maybe AudioRange,
    collection :: CollectionRef,
    cover :: Maybe PictureType
  }
  deriving (Show, Generic)
  deriving TextShow via FromStringShow MetadataImportSource

getSourceUri :: NewImportSource -> URI
getSourceUri (FileSource _ f) = fileUri f.filePath
getSourceUri (CueFileImportSource _ CueFileSource {..}) = fileUri filePath

getFileSourceUri :: MetadataFile -> URI
getFileSourceUri f = fileUri f.filePath

getAllMetadata :: NewImportSource -> [Metadata]
getAllMetadata (FileSource _ f) = H.elems $ getFileMetadata f
getAllMetadata (CueFileImportSource _ CueFileSource {..}) = [metadata]

getMetadata :: MetadataId -> NewImportSource -> Maybe Metadata
getMetadata mid (FileSource _ f) = H.lookup mid $ getFileMetadata f
getMetadata (MetadataId "CUE") (CueFileImportSource _ CueFileSource {..}) = Just metadata
getMetadata _ (CueFileImportSource _ CueFileSource {}) = Nothing

getFileMetadata :: MetadataFile -> H.HashMap MetadataId Metadata
getFileMetadata f = f.metadata

getInfo :: NewImportSource -> Info
getInfo (FileSource _ f) = getFileInfo f
getInfo (CueFileImportSource _ CueFileSource {..}) = audioInfo

getFileInfo :: MetadataFile -> Info
getFileInfo f = f.audioInfo

getMetadataFileId :: NewImportSource -> MetadataFileId
getMetadataFileId (FileSource _ f) = f.fileId
getMetadataFileId (CueFileImportSource _ CueFileSource {..}) = fileId

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
    collection :: CollectionRef,
    cover :: Maybe PictureType
  }
  deriving (Show, Eq, Generic)
  deriving TextShow via FromGeneric NewSource

data AudioRange = TimeRange (Range CalendarDiffTime)
  deriving (Eq, Show)
  deriving TextShow via FromStringShow AudioRange

instance From AudioRange IntervalRange where
  from (TimeRange r) = IntervalRange r

instance From IntervalRange AudioRange where
  from (IntervalRange r) = TimeRange r

nullMetadata :: MetadataId
nullMetadata = MetadataId "null"

instance From MetadataImportSource NewSource where
  from ms =
    NewSource
      { kind = coerce ms.metadataFileId,
        metadataFormat = coerce $ fromMaybe nullMetadata (ms.metadata <&> (.formatId)),
        tags = fromMaybe emptyTags (ms.metadata <&> (.tags)),
        source = showt $ ms.src,
        idx = ms.idx,
        range = ms.range,
        collection = ms.collection,
        cover = ms.cover
      }

newtype SourceRef = SourceRef {unSourceRef :: UUID}
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, DBType, DBEq, Hashable)
  deriving TextShow via FromGeneric SourceRef

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
    metadata :: Maybe Metadata,
    source :: URI,
    kind :: MetadataFileId,
    multiTrack :: Maybe MultiTrackDesc,
    collectionRef :: CollectionRef,
    length :: Maybe NominalDiffTime,
    cover :: Maybe PictureType
  }
  deriving (Generic, Show, Eq)
  deriving TextShow via FromGeneric Source

instance TryFrom SourceEntity Source where
  tryFrom s = maybeToRight (TryFromException s Nothing) do
    uri <- parseURI $ T.unpack s.source_uri
    let multiTrack = case (s.idx, s.time_range) of
          (idx, Just timeRange) | idx > -1 -> Just MultiTrackDesc {idx, range = from timeRange}
          _ -> Nothing
    pure
      Source
        { ref = s.id,
          multiTrack,
          source = uri,
          kind = MetadataFileId s.kind,
          collectionRef = s.collection_id,
          cover = coerce s.cover,
          length = s.time_range >>= rangeLength,
          metadata = metadataFromEntity s
        }

metadataFromEntity :: SourceEntity -> Maybe Metadata
metadataFromEntity s = let JSONBEncoded (SourceMetadata tags) = s.metadata
                           tags' = Tags $ tags <&> (\p -> (p.key, p.value))
                           mid = MetadataId s.metadata_format in
  mfilter (\m -> m.formatId /= nullMetadata) $ mkCueMetadata mid tags' <|> mkMetadata mid tags'

instance TryFrom SourceEntity Metadata where
  tryFrom e = maybeToRight (TryFromException e Nothing) (metadataFromEntity e)

mkCueMetadata :: MetadataId -> Tags -> Maybe Metadata
mkCueMetadata (MetadataId mid) tags
  | mid == "CUE" =
      Just $
        Metadata
          { tags,
            formatId = MetadataId mid,
            formatDesc = "CUE file",
            mappingSelector = Mapping.cue
          }
mkCueMetadata _ _ = Nothing

data MultiTrackDesc = MultiTrackDesc
  { idx :: Int16,
    range :: AudioRange
  }
  deriving (Generic, Show, Eq)
  deriving TextShow via FromGeneric MultiTrackDesc

instance From Source SourceEntity where
  from Source {..} =
    SourceTable
      { id = ref,
        kind = coerce kind,
        metadata_format = coerce $ fromMaybe nullMetadata (metadata ^? _Just . (to (.formatId))),
        metadata = JSONBEncoded $ from $ fromMaybe emptyTags (metadata ^? _Just . (to (.tags))),
        source_uri = showt source,
        idx = fromMaybe (-1) (multiTrack ^? _Just . #idx),
        time_range = (from <$> multiTrack ^? _Just . #range)
          <|> (IntervalRange . R.ubi . calendarTimeTime <$> length),
        scanned = unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = collectionRef,
        cover = coerce cover
      }

type UpdateSource = SourceEntity

data ImportStats = ImportStats
  { sourcesImported :: Natural,
    tracksImported :: Natural,
    releasesImported :: Natural,
    artistsImported :: Natural,
    genresImported :: Natural
  }
  deriving (Eq, Show, Generic)
  deriving TextShow via FromGeneric ImportStats

instance Semigroup ImportStats where
  a <> b =
    ImportStats
      { sourcesImported = a.sourcesImported + b.sourcesImported,
        tracksImported = a.tracksImported + b.tracksImported,
        releasesImported = a.releasesImported + b.releasesImported,
        artistsImported = a.artistsImported + b.artistsImported,
        genresImported = a.genresImported + b.genresImported
      }

instance Monoid ImportStats where
  mempty =
    ImportStats
      { sourcesImported = 0,
        tracksImported = 0,
        releasesImported = 0,
        artistsImported = 0,
        genresImported = 0
      }

data SourcePathPattern
  = LiteralPattern FilePath
  | GroupPattern (NonEmpty SourcePathPattern)
  | MappingPattern Text
  | DefaultPattern SourcePathPattern SourcePathPattern
  | PrintfPattern String SourcePathPattern
  deriving (Show, Eq)
  deriving TextShow via FromStringShow SourcePathPattern

data SourceFileManipError
  = FileSystemFileManipError FileManipError
  | PatternError
  | NoSuchSource
  | SourcePathError
  | SourceUpdateError
  | WrongCollection
  | ConversionError (TryFromException SourceEntity Source)
  deriving (Show)
  deriving TextShow via FromStringShow SourceFileManipError

instance From FileManipError SourceFileManipError where
  from = FileSystemFileManipError
