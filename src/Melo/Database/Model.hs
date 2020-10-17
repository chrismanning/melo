{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Database.Model where

import Control.Lens as L hiding ((.=))
import Data.Aeson as A
import Data.Aeson.Types (parseMaybe)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Lazy as L
import Data.Char
import Data.Fixed
import Data.Generics.Labels ()
import Data.Hashable
import Data.Int (Int16, Int64)
import Data.Kind
import Data.Text
import Data.Time.Clock
import Data.Time.LocalTime
import Data.UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Beam hiding (char, double)
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..))
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField as Pg
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.TypeInfo.Static (interval, jsonbOid)

data MeloDb f = MeloDb
  { source :: f (TableEntity SourceT),
    genre :: f (TableEntity GenreT),
    artist :: f (TableEntity ArtistT),
    artist_stage :: f (TableEntity ArtistStageT),
    album :: f (TableEntity AlbumT),
    album_stage :: f (TableEntity AlbumStageT),
    track :: f (TableEntity TrackT),
    track_stage :: f (TableEntity TrackStageT),
    related_artist :: f (TableEntity RelatedArtistT),
    track_genre :: f (TableEntity TrackGenreT),
    album_genre :: f (TableEntity AlbumGenreT),
    artist_genre :: f (TableEntity ArtistGenreT),
    track_artist :: f (TableEntity TrackArtistNameT),
    album_artist_name :: f (TableEntity AlbumArtistNameT),
    artist_name :: f (TableEntity ArtistNameT)
  }
  deriving (Generic, Database Postgres)

meloDb :: DatabaseSettings Postgres MeloDb
meloDb =
  defaultDbSettings
    `withDbModification` dbModification
      { album_artist_name =
          modifyTableFields @AlbumArtistNameT
            ( tableModification
                { album_id = AlbumKey $ fieldNamed "album_id",
                  artist_name_id = ArtistNameKey $ fieldNamed "artist_name_id"
                }
            ),
        track =
          modifyTableFields @TrackT
            ( tableModification
                { album_id = AlbumKey $ fieldNamed "album_id",
                  source_id = SourceKey $ fieldNamed "source_id"
                }
            ),
        track_stage =
          modifyTableFields @TrackStageT
            ( tableModification
                { album_id = AlbumKey $ fieldNamed "album_id",
                  source_id = SourceKey $ fieldNamed "source_id"
                }
            ),
        track_genre =
          modifyTableFields @TrackGenreT
            ( tableModification
                { track_id = TrackKey $ fieldNamed "track_id",
                  genre_id = GenreKey $ fieldNamed "genre_id"
                }
            )
      }

data GenreT (f :: Type -> Type) = Genre
  { id :: Columnar f UUID,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text)
  }
  deriving (Generic, Beamable)

instance Table GenreT where
  data PrimaryKey GenreT f
    = GenreKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = GenreKey . view #id

type Genre = GenreT Identity

type GenreKey = PrimaryKey GenreT Identity

deriving instance Show Genre

deriving instance Eq Genre

deriving instance Show GenreKey

deriving instance Eq GenreKey

deriving instance Ord GenreKey

deriving instance Hashable GenreKey

data ArtistT (f :: Type -> Type) = Artist
  { id :: Columnar f UUID,
    name :: Columnar f Text,
    disambiguation :: Columnar f (Maybe Text),
    short_bio :: Columnar f (Maybe Text),
    bio :: Columnar f (Maybe Text),
    country :: Columnar f (Maybe Text),
    musicbrainz_id :: Columnar f (Maybe Text)
  }
  deriving (Generic, Beamable)

instance Table ArtistT where
  newtype PrimaryKey ArtistT f
    = ArtistKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = ArtistKey . view #id

type Artist = ArtistT Identity

type ArtistKey = PrimaryKey ArtistT Identity

deriving instance Show Artist

deriving instance Eq Artist

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey ArtistT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey ArtistT (f :: Type -> Type))

deriving instance Ord ArtistKey

deriving newtype instance Hashable ArtistKey

data ArtistStageT (f :: Type -> Type) = ArtistStage
  { id :: Columnar f UUID,
    name :: Columnar f Text,
    disambiguation :: Columnar f (Maybe Text),
    short_bio :: Columnar f (Maybe Text),
    bio :: Columnar f (Maybe Text),
    country :: Columnar f (Maybe Text),
    musicbrainz_id :: Columnar f (Maybe Text),
    ref_artist_id :: PrimaryKey ArtistT (Nullable f),
    ref_album_id :: PrimaryKey AlbumT (Nullable f),
    ref_track_id :: PrimaryKey TrackT (Nullable f)
  }
  deriving (Generic, Beamable)

instance Table ArtistStageT where
  newtype PrimaryKey ArtistStageT f
    = ArtistStageKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = ArtistStageKey . view #id

type ArtistStage = ArtistStageT Identity

type ArtistStageKey = PrimaryKey ArtistStageT Identity

deriving instance Show ArtistStage

deriving instance Eq ArtistStage

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey ArtistStageT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey ArtistStageT (f :: Type -> Type))

deriving instance Ord ArtistStageKey

deriving newtype instance Hashable ArtistStageKey

data ArtistNameT (f :: Type -> Type) = ArtistName
  { artist_name_id :: Columnar f UUID,
    artist_id :: PrimaryKey ArtistT f,
    name :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table ArtistNameT where
  newtype PrimaryKey ArtistNameT f
    = ArtistNameKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = ArtistNameKey <$> view #artist_name_id

type ArtistName = ArtistNameT Identity

type ArtistNameKey = PrimaryKey ArtistNameT Identity

deriving instance Show ArtistName

deriving instance Eq ArtistName

deriving instance Show ArtistNameKey

deriving instance Eq ArtistNameKey

deriving instance Ord ArtistNameKey

deriving newtype instance Hashable ArtistNameKey

data RelatedArtistT (f :: Type -> Type) = RelatedArtist
  { artist_id :: PrimaryKey ArtistT f,
    related_artist_id :: PrimaryKey ArtistT f
  }
  deriving (Generic, Beamable)

instance Table RelatedArtistT where
  data PrimaryKey RelatedArtistT f
    = RelatedArtistKey (PrimaryKey ArtistT f) (PrimaryKey ArtistT f)
    deriving (Generic, Beamable)

  primaryKey = RelatedArtistKey <$> view #artist_id <*> view #related_artist_id

type RelatedArtist = RelatedArtistT Identity

type RelatedArtistKey = PrimaryKey RelatedArtistT Identity

deriving instance Show RelatedArtist

deriving instance Eq RelatedArtist

deriving instance Show RelatedArtistKey

deriving instance Eq RelatedArtistKey

deriving instance Ord RelatedArtistKey

deriving instance Hashable RelatedArtistKey

data AlbumT (f :: Type -> Type) = Album
  { id :: Columnar f UUID,
    title :: Columnar f Text,
    comment :: Columnar f (Maybe Text),
    year_released :: Columnar f (Maybe Text),
    length :: Columnar f Interval,
    musicbrainz_id :: Columnar f (Maybe Text)
  }
  deriving (Generic, Beamable)

instance Table AlbumT where
  newtype PrimaryKey AlbumT f
    = AlbumKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = AlbumKey . view #id

type Album = AlbumT Identity

type AlbumKey = PrimaryKey AlbumT Identity

deriving instance Show Album

deriving instance Eq Album

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey AlbumT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey AlbumT (f :: Type -> Type))

deriving instance Ord AlbumKey

deriving newtype instance Hashable AlbumKey

data AlbumStageT (f :: Type -> Type) = AlbumStage
  { id :: Columnar f UUID,
    title :: Columnar f Text,
    comment :: Columnar f (Maybe Text),
    year_released :: Columnar f (Maybe Text),
    length :: Columnar f Interval,
    ref_artist_id :: PrimaryKey ArtistT (Nullable f),
    ref_album_id :: PrimaryKey AlbumT (Nullable f),
    ref_track_id :: PrimaryKey TrackT (Nullable f)
  }
  deriving (Generic, Beamable)

instance Table AlbumStageT where
  data PrimaryKey AlbumStageT f
    = AlbumStageKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = AlbumStageKey . view #id

type AlbumStage = AlbumStageT Identity

type AlbumStageKey = PrimaryKey AlbumStageT Identity

deriving instance Show AlbumStage

deriving instance Eq AlbumStage

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey AlbumStageT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey AlbumStageT (f :: Type -> Type))

deriving instance Ord AlbumStageKey

deriving instance Hashable AlbumStageKey

data AlbumArtistNameT (f :: Type -> Type) = AlbumArtistName
  { album_id :: PrimaryKey AlbumT f,
    artist_name_id :: PrimaryKey ArtistNameT f
  }
  deriving (Generic, Beamable)

instance Table AlbumArtistNameT where
  data PrimaryKey AlbumArtistNameT f
    = AlbumArtistNameKey (PrimaryKey AlbumT f) (PrimaryKey ArtistNameT f)
    deriving (Generic, Beamable)

  primaryKey = AlbumArtistNameKey <$> view #album_id <*> view #artist_name_id

type AlbumArtistName = AlbumArtistNameT Identity

type AlbumArtistNameKey = PrimaryKey AlbumArtistNameT Identity

deriving instance Show AlbumArtistName

deriving instance Eq AlbumArtistName

deriving instance Show AlbumArtistNameKey

deriving instance Eq AlbumArtistNameKey

deriving instance Ord AlbumArtistNameKey

deriving instance Hashable AlbumArtistNameKey

albumArtistNameRelationship :: ManyToMany Postgres MeloDb ArtistNameT AlbumT
albumArtistNameRelationship =
  manyToMany_ (meloDb ^. #album_artist_name) (^. #artist_name_id) (^. #album_id)

data TrackT (f :: Type -> Type) = Track
  { id :: Columnar f UUID,
    title :: Columnar f Text,
    album_id :: PrimaryKey AlbumT f,
    track_number :: Columnar f (Maybe Int16),
    disc_number :: Columnar f (Maybe Int16),
    comment :: Columnar f (Maybe Text),
    source_id :: PrimaryKey SourceT f,
    length :: Columnar f Interval
  }
  deriving (Generic, Beamable)

instance Table TrackT where
  newtype PrimaryKey TrackT f
    = TrackKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = TrackKey . view #id

type Track = TrackT Identity

type TrackKey = PrimaryKey TrackT Identity

deriving instance Show Track

deriving instance Eq Track

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey TrackT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey TrackT (f :: Type -> Type))

deriving instance Ord TrackKey

deriving newtype instance Hashable TrackKey

data TrackStageT (f :: Type -> Type) = TrackStage
  { id :: Columnar f UUID,
    title :: Columnar f (Maybe Text),
    album_id :: PrimaryKey AlbumT (Nullable f),
    track_number :: Columnar f (Maybe Int),
    disc_number :: Columnar f (Maybe Int),
    comment :: Columnar f (Maybe Text),
    source_id :: PrimaryKey SourceT (Nullable f),
    length :: Columnar f (Maybe Interval),
    ref_artist_id :: PrimaryKey ArtistT (Nullable f),
    ref_album_id :: PrimaryKey AlbumT (Nullable f),
    ref_track_id :: PrimaryKey TrackT (Nullable f)
  }
  deriving (Generic, Beamable)

instance Table TrackStageT where
  newtype PrimaryKey TrackStageT f
    = TrackStageKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = TrackStageKey . view #id

type TrackStage = TrackStageT Identity

type TrackStageKey = PrimaryKey TrackStageT Identity

deriving instance Show TrackStage

deriving instance Eq TrackStage

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey TrackStageT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey TrackStageT (f :: Type -> Type))

deriving instance Ord TrackStageKey

deriving newtype instance Hashable TrackStageKey

data SourceT (f :: Type -> Type) = Source
  { id :: Columnar f UUID,
    kind :: Columnar f Text,
    metadata_format :: Columnar f Text,
    metadata :: Columnar f (PgJSONB SourceMetadata),
    source_uri :: Columnar f Text,
    idx :: Columnar f Int16,
    time_range :: Columnar f (Maybe (PgRange IntervalRange Interval)),
    sample_range :: Columnar f (Maybe (PgRange PgInt8Range Int64)),
    scanned :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

instance Table SourceT where
  newtype PrimaryKey SourceT f
    = SourceKey (Columnar f UUID)
    deriving (Generic)
    deriving anyclass (Beamable)

  primaryKey = SourceKey . view #id

type Source = SourceT Identity

type SourceKey = PrimaryKey SourceT Identity

deriving instance Show Source

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey SourceT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey SourceT (f :: Type -> Type))

deriving instance Ord SourceKey

deriving newtype instance Hashable SourceKey

newtype SourceMetadata = SourceMetadata (Vector (Text, Text))
  deriving (Show, Eq)

instance Pg.FromField SourceMetadata where
  fromField field d =
    if Pg.typeOid field /= jsonbOid
      then Pg.returnError Pg.Incompatible field ""
      else case eitherDecodeStrict' <$> d of
        Nothing -> Pg.returnError Pg.UnexpectedNull field ""
        Just (Left e) -> Pg.returnError Pg.ConversionFailed field e
        Just (Right d') -> pure d'

instance FromBackendRow Postgres SourceMetadata

instance HasSqlValueSyntax PgValueSyntax SourceMetadata where
  sqlValueSyntax a =
    PgValueSyntax $
      emit "'" <> escapeString (L.toStrict (encode (toJSON a))) <> emit "'::jsonb"

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

data ArtistGenreT (f :: Type -> Type) = ArtistGenre
  { artist_id :: PrimaryKey ArtistT f,
    genre_id :: PrimaryKey GenreT f
  }
  deriving (Generic, Beamable)

instance Table ArtistGenreT where
  data PrimaryKey ArtistGenreT f
    = ArtistGenreKey (PrimaryKey ArtistT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)

  primaryKey = ArtistGenreKey <$> view #artist_id <*> view #genre_id

type ArtistGenre = ArtistGenreT Identity

type ArtistGenreKey = PrimaryKey ArtistGenreT Identity

deriving instance Show ArtistGenre

deriving instance Eq ArtistGenre

deriving instance Show ArtistGenreKey

deriving instance Eq ArtistGenreKey

deriving instance Ord ArtistGenreKey

deriving instance Hashable ArtistGenreKey

data AlbumGenreT (f :: Type -> Type) = AlbumGenre
  { album_id :: PrimaryKey AlbumT f,
    genre_id :: PrimaryKey GenreT f
  }
  deriving (Generic, Beamable)

instance Table AlbumGenreT where
  data PrimaryKey AlbumGenreT f
    = AlbumGenreKey (PrimaryKey AlbumT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)

  primaryKey = AlbumGenreKey <$> view #album_id <*> view #genre_id

type AlbumGenre = AlbumGenreT Identity

type AlbumGenreKey = PrimaryKey AlbumGenreT Identity

deriving instance Show AlbumGenre

deriving instance Eq AlbumGenre

deriving instance Show AlbumGenreKey

deriving instance Eq AlbumGenreKey

deriving instance Ord AlbumGenreKey

deriving instance Hashable AlbumGenreKey

data TrackArtistNameT (f :: Type -> Type) = TrackArtistName
  { track_id :: PrimaryKey TrackT f,
    artist_id :: PrimaryKey ArtistT f
  }
  deriving (Generic, Beamable)

instance Table TrackArtistNameT where
  data PrimaryKey TrackArtistNameT f
    = TrackArtistNameKey (PrimaryKey TrackT f) (PrimaryKey ArtistT f)
    deriving (Generic, Beamable)

  primaryKey = TrackArtistNameKey <$> view #track_id <*> view #artist_id

type TrackArtistName = TrackArtistNameT Identity

type TrackArtistNameKey = PrimaryKey TrackArtistNameT Identity

deriving instance Show TrackArtistName

deriving instance Eq TrackArtistName

deriving instance Show TrackArtistNameKey

deriving instance Eq TrackArtistNameKey

deriving instance Ord TrackArtistNameKey

deriving instance Hashable TrackArtistNameKey

data TrackGenreT (f :: Type -> Type) = TrackGenre
  { track_id :: PrimaryKey TrackT f,
    genre_id :: PrimaryKey GenreT f
  }
  deriving (Generic, Beamable)

instance Table TrackGenreT where
  data PrimaryKey TrackGenreT f
    = TrackGenreKey (PrimaryKey TrackT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)

  primaryKey = TrackGenreKey <$> view #track_id <*> view #genre_id

type TrackGenre = TrackGenreT Identity

type TrackGenreKey = PrimaryKey TrackGenreT Identity

deriving instance Show TrackGenre

deriving instance Eq TrackGenre

deriving instance Show TrackGenreKey

deriving instance Eq TrackGenreKey

deriving instance Ord TrackGenreKey

deriving instance Hashable TrackGenreKey

data IntervalRange

instance PgIsRange IntervalRange where
  rangeName = "intervalrange"

newtype Interval = Interval NominalDiffTime
  deriving (Show, Eq, Ord)

mkInterval :: NominalDiffTime -> Maybe Interval
-- TODO check if NominalDiffTime fits in Interval
mkInterval = Just . Interval

instance FromBackendRow Postgres Interval

instance FromField Interval where
  fromField f mdat =
    if typeOid f /= typoid interval
      then returnError Incompatible f ""
      else case mdat of
        Nothing -> returnError UnexpectedNull f ""
        Just dat -> case parseOnly (Interval <$> nominalDiffTimeParser <* endOfInput) dat of
          Left msg -> returnError ConversionFailed f msg
          Right t -> return t

instance ToField Interval where
  toField (Interval i) = Many [toField i, Plain " :: INTERVAL"]

instance HasSqlValueSyntax PgValueSyntax Interval where
  sqlValueSyntax = defaultPgValueSyntax

nominalDiffTimeParser :: Parser NominalDiffTime
nominalDiffTimeParser = do
  (h, m, s) <- intervalParser
  return . fromRational . toRational $ s + 60 * (fromIntegral m) + 60 * 60 * (fromIntegral h)

-- | Parse a limited postgres interval of the form [-]HHH:MM:SS.[SSSS] (no larger units than hours).
intervalParser :: Parser (Int, Int, Pico)
intervalParser = do
  h <- signed decimal <* char ':'
  m <- twoDigits <* char ':'
  s <- rational
  if m < 60 && s <= 60
    then return (h, m, s)
    else fail "invalid interval"

twoDigits :: Parser Int
twoDigits = do
  a <- ord <$> digit
  b <- ord <$> digit
  pure $ a * 10 + b
