{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Source.Types where

import BinaryParser as BP
import Control.Applicative
import Control.DeepSeq
import Control.Lens (to)
import Control.Monad
import Data.Aeson hiding (Result)
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Bits
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Coerce
import Data.Either.Combinators
import Data.Fixed
import Data.HashMap.Strict qualified as H
import Data.Hashable
import Data.Int
import Data.Range (Range (..))
import Data.Range qualified as R
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock (NominalDiffTime ())
import Data.Time.Format
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
    Decoder (..),
    Expr,
    JSONBEncoded (..),
    ReadShow (..),
    Rel8able,
    Result,
    TypeInformation (..),
    function,
    lit,
  )
import System.IO.Unsafe
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

deriving via CustomJSON JSONOptions SourceEntity instance ToJSON SourceEntity

instance Entity SourceEntity where
  type NewEntity SourceEntity = NewSource
  type PrimaryKey SourceEntity = SourceRef
  primaryKey e = e.id

newtype PictureTypeWrapper = PictureTypeWrapper PictureType
  deriving (Generic, Show, Eq)
  deriving (DBType) via ReadShow PictureType
  deriving (TextShow) via FromGeneric PictureTypeWrapper
  deriving newtype (ToJSON)

newtype IntervalRange = IntervalRange (Range CalendarDiffTime)
  deriving (Show, Eq)
  deriving (TextShow) via FromStringShow IntervalRange

instance ToJSON IntervalRange where
  toJSON (IntervalRange range) = toJSON (show range)
  toEncoding (IntervalRange range) = toEncoding (show range)

rangeLength :: IntervalRange -> Maybe NominalDiffTime
rangeLength (IntervalRange r) = rangeLength' r
  where
    rangeLength' (SpanRange lower upper) = Just $ upper.boundValue.ctTime - lower.boundValue.ctTime
    rangeLength' (UpperBoundRange upper) = Just upper.boundValue.ctTime
    rangeLength' _ = Nothing

instance DBType IntervalRange where
  typeInformation =
    TypeInformation
      { encode,
        decode,
        typeName = "intervalrange"
      }
    where
      encode :: IntervalRange -> PrimExpr
      encode (IntervalRange (SingletonRange a)) = RangeExpr "intervalrange" (Inclusive (encodeInterval a)) (Inclusive (encodeInterval a))
      encode (IntervalRange (SpanRange a b)) = RangeExpr "intervalrange" (encodeBound a) (encodeBound b)
      encode (IntervalRange (LowerBoundRange a)) = RangeExpr "intervalrange" (encodeBound a) PosInfinity
      encode (IntervalRange (UpperBoundRange a)) = RangeExpr "intervalrange" NegInfinity (encodeBound a)
      encode (IntervalRange InfiniteRange) = RangeExpr "intervalrange" NegInfinity PosInfinity
      encodeInterval :: CalendarDiffTime -> PrimExpr
      encodeInterval = CastExpr "interval" . ConstExpr . StringLit . formatTime defaultTimeLocale "'%bmon %0Es'"
      encodeBound R.Bound {boundValue, boundType = R.Inclusive} = Inclusive (encodeInterval boundValue)
      encodeBound R.Bound {boundValue, boundType = R.Exclusive} = Exclusive (encodeInterval boundValue)
      decode :: Rel8.Decoder (IntervalRange)
      decode =
        Rel8.Decoder
          { binary = Hasql.custom decodeRange,
            parser = P.parseOnly (parser' <* P.endOfInput),
            delimiter = ','
          }
      parser' = do
        P.skipSpace
        lowerBoundType <-
          P.char '[' <|> P.char '(' >>= \case
            '[' -> pure R.Inclusive
            '(' -> pure R.Exclusive
            _ -> error "unknown bound"
        P.skipSpace
        lowerBound <-
          P.peekChar >>= \case
            Just ',' -> pure Nothing
            _ -> do
              lowerBound <- calendarDiffTimeParser
              pure $ Just $ R.Bound lowerBound lowerBoundType
        P.skipSpace
        upperBound <-
          P.peekChar >>= \case
            Just ']' -> pure Nothing
            Just ')' -> pure Nothing
            _ -> do
              upperBound <- calendarDiffTimeParser
              upperBoundType <-
                P.char ']' <|> P.char ')' >>= \case
                  ']' -> pure R.Inclusive
                  ')' -> pure R.Exclusive
                  _ -> error "unknown bound"
              pure $ Just $ R.Bound upperBound upperBoundType
        pure $ IntervalRange $ case (lowerBound, upperBound) of
          (Just lower, Just upper) -> R.SpanRange lower upper
          (Just lower, Nothing) -> R.LowerBoundRange lower
          (Nothing, Just upper) -> R.UpperBoundRange upper
          (Nothing, Nothing) -> R.InfiniteRange
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
      calendarDiffTimeParser :: P.Parser CalendarDiffTime
      calendarDiffTimeParser = iso8601 <|> postgres
        where
          iso8601 = P.takeByteString >>= iso8601ParseM . T.unpack . T.decodeUtf8Lenient
          at = optional (P.char '@') *> P.skipSpace
          plural unit = P.skipSpace <* (unit <* optional "s") <* P.skipSpace
          parseMonths = sql <|> postgresql
            where
              sql = P.signed $ do
                years <- P.decimal <* P.char '-'
                months <- P.decimal <* P.skipSpace
                pure $ years * 12 + months
              postgresql = do
                at
                years <- P.signed P.decimal <* plural "year" <|> pure 0
                months <- P.signed P.decimal <* plural "mon" <|> pure 0
                pure $ years * 12 + months
          parseTime = (+) <$> parseDays <*> time
            where
              time = realToFrac <$> (sql <|> postgresql)
                where
                  sql = P.signed $ do
                    h <- P.signed P.decimal <* P.char ':'
                    m <- twoDigits <* P.char ':'
                    s <- secondsParser
                    pure $ fromIntegral (((h * 60) + m) * 60) + s
                  postgresql = do
                    h <- P.signed P.decimal <* plural "hour" <|> pure 0
                    m <- P.signed P.decimal <* plural "min" <|> pure 0
                    s <- secondsParser <* plural "sec" <|> pure 0
                    pure $ fromIntegral @Int (((h * 60) + m) * 60) + s
              parseDays = do
                days <- P.signed P.decimal <* (plural "days" <|> skipSpace1) <|> pure 0
                pure $ fromIntegral @Int days * 24 * 60 * 60
          postgres = do
            months <- parseMonths
            time <- parseTime
            ago <- (True <$ (P.skipSpace *> "ago")) <|> pure False
            pure $ CalendarDiffTime (bool Prelude.id negate ago months) (bool Prelude.id negate ago time)
          skipSpace1 :: P.Parser ()
          skipSpace1 = void $ P.takeWhile1 P.isSpace
          twoDigits :: P.Parser Int
          twoDigits = do
            u <- P.digit
            l <- P.digit
            pure $ fromEnum u .&. 0xf * 10 + fromEnum l .&. 0xf
          secondsParser :: P.Parser Pico
          secondsParser = do
            integral <- twoDigits
            mfractional <- optional (P.char '.' *> P.takeWhile1 P.isDigit)
            pure $ case mfractional of
              Nothing -> fromIntegral integral
              Just fractional -> parseFraction (fromIntegral integral) fractional
            where
              parseFraction integral digits = MkFixed (fromIntegral (n * 10 ^ e))
                where
                  e = max 0 (12 - B.length digits)
                  n = B.foldl' go (integral :: Int64) (B.take 12 digits)
                    where
                      go acc digit = 10 * acc + fromIntegral (fromEnum digit .&. 0xf)

instance From NewSource (SourceTable Expr) where
  from s =
    SourceTable
      { id = function "uuid_generate_v4" (),
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
  deriving (TextShow) via FromGeneric SourceMetadata
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions SourceMetadata

instance From Tags SourceMetadata where
  from (Tags tags) = SourceMetadata (uncurry TagPair <$> tags)

instance From SourceMetadata Tags where
  from (SourceMetadata tags) = Tags (tags <&> (\p -> (p.key, p.value)))

data TagPair = TagPair
  { key :: Text,
    value :: Text
  }
  deriving (Show, Eq, Generic)
  deriving (TextShow) via FromGeneric TagPair
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions TagPair

data NewImportSource
  = FileSource CollectionRef MetadataFile
  | CueFileImportSource CollectionRef CueFileSource
  deriving (Eq, Show)
  deriving (TextShow) via FromStringShow NewImportSource

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
  deriving (TextShow) via FromGeneric CueFileSource

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
  deriving (TextShow) via FromStringShow MetadataImportSource

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
  deriving (TextShow) via FromGeneric NewSource

data AudioRange = TimeRange (Range CalendarDiffTime)
  deriving (Eq, Show)
  deriving (TextShow) via FromStringShow AudioRange

instance ToJSON AudioRange where
  toJSON (TimeRange range) = toJSON (show range)
  toEncoding (TimeRange range) = toEncoding (show range)

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
  deriving newtype (Show, Eq, Ord, DBType, DBEq, FromJSON, Hashable, ToJSON)
  deriving (TextShow) via FromGeneric SourceRef

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
    cover :: Maybe Image
  }
  deriving (Generic, Eq)
  deriving (TextShow) via FromGeneric Source
  deriving (ToJSON) via CustomJSON JSONOptions Source

instance TryFrom SourceEntity Source where
  tryFrom s = maybeToRight (TryFromException s Nothing) do
    uri <- parseURI $ T.unpack s.source_uri
    pure
      Source
        { ref = s.id,
          multiTrack,
          source = uri,
          kind = MetadataFileId s.kind,
          collectionRef = s.collection_id,
          cover,
          length = s.time_range >>= rangeLength,
          metadata = metadataFromEntity s
        }
    where
      multiTrack = case (s.idx, s.time_range) of
        (idx, Just timeRange) | idx > -1 -> Just MultiTrackDesc {idx, range = from timeRange}
        _ -> Nothing
      cover = case s.cover of
        Just cover -> Just $ Image {fileName = Nothing, imageType = (Just cover)}
        Nothing -> Nothing

metadataFromEntity :: SourceEntity -> Maybe Metadata
metadataFromEntity s =
  let JSONBEncoded (SourceMetadata tags) = s.metadata
      tags' = Tags $ tags <&> (\p -> (p.key, p.value))
      mid = MetadataId s.metadata_format
   in mfilter (\m -> m.formatId /= nullMetadata) $ mkCueMetadata mid tags' <|> mkMetadata mid tags'

instance TryFrom SourceEntity Metadata where
  tryFrom e = maybeToRight (TryFromException e Nothing) (metadataFromEntity e)

data SourceGroup = SourceGroup
  { groupTags :: MappedTags,
    groupParentUri :: Text,
    sources :: Vector Source,
    coverImage :: Maybe Image
  }
  deriving (Eq, Generic)
  deriving (TextShow) via FromGeneric SourceGroup
  deriving (ToJSON) via CustomJSON JSONOptions SourceGroup

type MappedTags = Vector MappedTag

data MappedTag = MappedTag
  { mappingName :: Text,
    values :: Vector Text
  }
  deriving (Show, Eq, Ord, Generic, NFData)
  deriving (TextShow) via FromGeneric MappedTag
  deriving (ToJSON) via CustomJSON JSONOptions MappedTag

data Image = Image
  { fileName :: Maybe Text,
    imageType :: Maybe PictureTypeWrapper
  }
  deriving (Eq, Generic)
  deriving (TextShow) via FromGeneric Image
  deriving (ToJSON) via CustomJSON JSONOptions Image

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
  deriving (TextShow) via FromGeneric MultiTrackDesc
  deriving (ToJSON) via CustomJSON JSONOptions MultiTrackDesc

instance From Source SourceEntity where
  from Source {..} =
    SourceTable
      { id = ref,
        kind = coerce kind,
        metadata_format = coerce $ fromMaybe nullMetadata (metadata ^? _Just . (to (.formatId))),
        metadata = JSONBEncoded $ from $ fromMaybe emptyTags (metadata ^? _Just . (to (.tags))),
        source_uri = showt source,
        idx = fromMaybe (-1) (multiTrack ^? _Just . #idx),
        time_range =
          (from <$> multiTrack ^? _Just . #range)
            <|> (IntervalRange . R.ubi . calendarTimeTime <$> length),
        scanned = unsafeDupablePerformIO getCurrentLocalTime,
        collection_id = collectionRef,
        cover = cover >>= (.imageType)
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
  deriving (TextShow) via FromGeneric ImportStats

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
  = LiteralPattern Text
  | GroupPattern (NonEmpty SourcePathPattern)
  | MappingPattern Text
  | DefaultPattern SourcePathPattern SourcePathPattern
  | PrintfPattern String SourcePathPattern
  deriving (Generic, Eq)
  deriving (TextShow) via FromGeneric SourcePathPattern
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions SourcePathPattern

data SourceFileManipError
  = FileSystemFileManipError FileManipError
  | PatternError
  | NoSuchSource
  | SourcePathError
  | SourceUpdateError
  | WrongCollection
  | ConversionError (TryFromException SourceEntity Source)
  deriving (Show)
  deriving (TextShow) via FromStringShow SourceFileManipError

instance From FileManipError SourceFileManipError where
  from = FileSystemFileManipError
