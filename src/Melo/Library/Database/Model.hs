{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Database.Model where

import Control.Lens as L
import Data.Aeson
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.Fixed
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Generics.Sum ()
import Data.Hashable
import Data.Int (Int64)
import Data.Kind
import Data.Text
import Data.Time.Clock
import Data.Time.LocalTime
import Data.UUID
import Database.Beam hiding (char, double)
import Database.Beam.Backend.SQL.SQL92 (HasSqlValueSyntax (..))
import Database.Beam.Postgres
import Database.Beam.Postgres.Syntax
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.TypeInfo.Static (interval)
import Debug.Trace
import GHC.Generics ()
import GHC.OverloadedLabels ()

data LibraryDb f = LibraryDb
  { source :: f (TableEntity SourceT),
    genre :: f (TableEntity GenreT),
    artist :: f (TableEntity ArtistT),
    album :: f (TableEntity AlbumT),
    track :: f (TableEntity TrackT),
    related_artist :: f (TableEntity RelatedArtistT),
    track_genre :: f (TableEntity TrackGenreT),
    album_genre :: f (TableEntity AlbumGenreT),
    artist_genre :: f (TableEntity ArtistGenreT),
    track_artist :: f (TableEntity TrackArtistNameT),
    album_artist_name :: f (TableEntity AlbumArtistNameT),
    artist_name :: f (TableEntity ArtistNameT)
  }
  deriving (Generic, Database be)

libraryDb :: DatabaseSettings be LibraryDb
libraryDb =
  defaultDbSettings
    `withDbModification` dbModification
      { album_artist_name =
          modifyTableFields
            ( tableModification
                & (setField @"album_id" (AlbumKey $ fieldNamed "album_id"))
                . (setField @"artist_name_id" (ArtistNameKey $ fieldNamed "artist_name_id"))
            ),
        track =
          modifyTableFields
            ( tableModification
                & (setField @"album_id" (AlbumKey $ fieldNamed "album_id"))
                . (setField @"source_id" (SourceKey $ fieldNamed "source_id"))
            ),
        track_genre =
          modifyTableFields
            ( tableModification
                & (setField @"track_id" (TrackKey $ fieldNamed "track_id"))
                . (setField @"genre_id" (GenreKey $ fieldNamed "genre_id"))
            )
      }

data GenreT (f :: Type -> Type) = Genre
  { id :: Columnar f UUID,
    name :: Columnar f Text,
    description :: Columnar f (Maybe Text)
    -- , links :: Columnar f (Maybe (PgJSONB Links))
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
    country :: Columnar f (Maybe Text)
  }
  deriving (Generic, Beamable)

instance Table ArtistT where
  data PrimaryKey ArtistT f
    = ArtistKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = ArtistKey . view #id

type Artist = ArtistT Identity

type ArtistKey = PrimaryKey ArtistT Identity

deriving instance Show Artist

deriving instance Eq Artist

deriving instance Show ArtistKey

deriving instance Eq ArtistKey

deriving instance Ord ArtistKey

deriving instance Hashable ArtistKey

data ArtistNameT (f :: Type -> Type) = ArtistName
  { artist_name_id :: Columnar f UUID,
    artist_id :: PrimaryKey ArtistT f,
    name :: Columnar f Text
  }
  deriving (Generic, Beamable)

instance Table ArtistNameT where
  data PrimaryKey ArtistNameT f
    = ArtistNameKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = ArtistNameKey <$> view #artist_name_id

type ArtistName = ArtistNameT Identity

type ArtistNameKey = PrimaryKey ArtistNameT Identity

deriving instance Show ArtistName

deriving instance Eq ArtistName

deriving instance Show ArtistNameKey

deriving instance Eq ArtistNameKey

deriving instance Ord ArtistNameKey

deriving instance Hashable ArtistNameKey

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
    length :: Columnar f Interval
  }
  deriving (Generic, Beamable)

instance Table AlbumT where
  data PrimaryKey AlbumT f
    = AlbumKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = AlbumKey . view #id

type Album = AlbumT Identity

type AlbumKey = PrimaryKey AlbumT Identity

deriving instance Show Album

deriving instance Eq Album

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey AlbumT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey AlbumT (f :: Type -> Type))

deriving instance Ord AlbumKey

deriving instance Hashable AlbumKey

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

albumArtistNameRelationship :: ManyToMany be LibraryDb ArtistNameT AlbumT
albumArtistNameRelationship =
  manyToMany_ (libraryDb ^. #album_artist_name) (^. #artist_name_id) (^. #album_id)

data TrackT (f :: Type -> Type) = Track
  { id :: Columnar f UUID,
    title :: Columnar f Text,
    album_id :: PrimaryKey AlbumT f,
    track_number :: Columnar f (Maybe Int),
    disc_number :: Columnar f (Maybe Int),
    comment :: Columnar f (Maybe Text),
    source_id :: PrimaryKey SourceT f,
    length :: Columnar f Interval
  }
  deriving (Generic, Beamable)

instance Table TrackT where
  data PrimaryKey TrackT f
    = TrackKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = TrackKey . view #id

type Track = TrackT Identity

type TrackKey = PrimaryKey TrackT Identity

deriving instance Show Track

deriving instance Eq Track

deriving instance Show TrackKey

deriving instance Eq TrackKey

deriving instance Ord TrackKey

deriving instance Hashable TrackKey

data SourceT (f :: Type -> Type) = Source
  { id :: Columnar f UUID,
    kind :: Columnar f Text,
    metadata_format :: Columnar f Text,
    metadata :: Columnar f (PgJSONB Value),
    source_uri :: Columnar f Text,
    idx :: Columnar f Int,
    time_range :: Columnar f (Maybe (PgRange IntervalRange Interval)),
    sample_range :: Columnar f (Maybe (PgRange PgInt8Range Int64)),
    scanned :: Columnar f LocalTime
  }
  deriving (Generic, Beamable)

instance Table SourceT where
  data PrimaryKey SourceT f
    = SourceKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = SourceKey . view #id

type Source = SourceT Identity

type SourceKey = PrimaryKey SourceT Identity

deriving instance Show Source

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey SourceT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey SourceT (f :: Type -> Type))

deriving instance Ord SourceKey

deriving instance Hashable SourceKey

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
  traceM $ "h: " <> show h <> "; m: " <> show m <> "; s: " <> show s
  if m < 60 && s <= 60
    then return (h, m, s)
    else fail "invalid interval"

twoDigits :: Parser Int
twoDigits = do
  a <- ord <$> digit
  b <- ord <$> digit
  pure $ a * 10 + b
