{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Database.Model where

import Control.Lens as L
import Data.Attoparsec.ByteString.Char8
import Data.Char
import Data.Fixed
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Generics.Sum ()
import Data.Hashable
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
import GHC.Generics ()
import GHC.OverloadedLabels ()

data LibraryDb f
  = LibraryDb
      { audio_source :: f (TableEntity AudioSourceT),
        metadata_source :: f (TableEntity MetadataSourceT),
        genre :: f (TableEntity GenreT),
        artist :: f (TableEntity ArtistT),
        album :: f (TableEntity AlbumT),
        track :: f (TableEntity TrackT),
        related_artist :: f (TableEntity RelatedArtistT),
        track_genre :: f (TableEntity TrackGenreT),
        album_genre :: f (TableEntity AlbumGenreT),
        artist_genre :: f (TableEntity ArtistGenreT),
        track_artist :: f (TableEntity TrackArtistAliasT),
        album_artist_alias :: f (TableEntity AlbumArtistAliasT),
        artist_alias :: f (TableEntity ArtistAliasT)
      }
  deriving (Generic, Database be)

libraryDb :: DatabaseSettings be LibraryDb
libraryDb =
  defaultDbSettings
    `withDbModification` dbModification
      { album_artist_alias =
          modifyTableFields
            ( tableModification
                & (setField @"album_id" (AlbumKey $ fieldNamed "album_id"))
                . (setField @"artist_alias_id" (ArtistAliasKey $ fieldNamed "artist_alias_id"))
            ),
        track =
          modifyTableFields
            ( tableModification
                & (setField @"album_id" (AlbumKey $ fieldNamed "album_id"))
                . (setField @"metadata_source_id" (MetadataSourceKey $ fieldNamed "metadata_source_id"))
                . (setField @"audio_source_id" (AudioSourceKey $ fieldNamed "audio_source_id"))
            ),
        track_genre =
          modifyTableFields
            ( tableModification
                & (setField @"track_id" (TrackKey $ fieldNamed "track_id"))
                . (setField @"genre_id" (GenreKey $ fieldNamed "genre_id"))
            )
      }

data GenreT (f :: Type -> Type)
  = Genre
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

data ArtistT (f :: Type -> Type)
  = Artist
      { id :: Columnar f UUID,
        name :: Columnar f Text,
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

data ArtistAliasT (f :: Type -> Type)
  = ArtistAlias
      { alias_id :: Columnar f UUID,
        artist_id :: PrimaryKey ArtistT f,
        alias :: Columnar f Text
      }
  deriving (Generic, Beamable)

instance Table ArtistAliasT where
  data PrimaryKey ArtistAliasT f
    = ArtistAliasKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = ArtistAliasKey <$> view #alias_id

type ArtistAlias = ArtistAliasT Identity

type ArtistAliasKey = PrimaryKey ArtistAliasT Identity

data RelatedArtistT (f :: Type -> Type)
  = RelatedArtist
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

data AlbumT (f :: Type -> Type)
  = Album
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

data AlbumArtistAliasT (f :: Type -> Type)
  = AlbumArtistAlias
      { album_id :: PrimaryKey AlbumT f,
        artist_alias_id :: PrimaryKey ArtistAliasT f
      }
  deriving (Generic, Beamable)

instance Table AlbumArtistAliasT where
  data PrimaryKey AlbumArtistAliasT f
    = AlbumArtistAliasKey (PrimaryKey AlbumT f) (PrimaryKey ArtistAliasT f)
    deriving (Generic, Beamable)

  primaryKey = AlbumArtistAliasKey <$> view #album_id <*> view #artist_alias_id

albumArtistAliasRelationship :: ManyToMany be LibraryDb ArtistAliasT AlbumT
albumArtistAliasRelationship =
  manyToMany_ (libraryDb ^. #album_artist_alias) (^. #artist_alias_id) (^. #album_id)

data TrackT (f :: Type -> Type)
  = Track
      { id :: Columnar f UUID,
        title :: Columnar f Text,
        album_id :: PrimaryKey AlbumT (Nullable f),
        track_number :: Columnar f (Maybe Int),
        disc_number :: Columnar f (Maybe Int),
        comment :: Columnar f (Maybe Text),
        audio_source_id :: PrimaryKey AudioSourceT (Nullable f),
        metadata_source_id :: PrimaryKey MetadataSourceT f,
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

data AudioSourceT (f :: Type -> Type)
  = AudioSource
      { id :: Columnar f UUID,
        kind :: Columnar f Text,
        source :: Columnar f Text,
        format :: Columnar f Text,
        sample_rate :: Columnar f Int,
        bits_per_sample :: Columnar f Int,
        channels :: Columnar f Int,
        total_samples :: Columnar f Integer
      }
  deriving (Generic, Beamable)

instance Table AudioSourceT where
  data PrimaryKey AudioSourceT f
    = AudioSourceKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = AudioSourceKey . view #id

type AudioSource = AudioSourceT Identity

type AudioSourceKey = PrimaryKey AudioSourceT Identity

deriving instance Show AudioSource

deriving instance Eq AudioSource

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey AudioSourceT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey AudioSourceT (f :: Type -> Type))

data MetadataSourceT (f :: Type -> Type)
  = MetadataSource
      { id :: Columnar f UUID,
        kind :: Columnar f Text,
        source :: Columnar f Text,
        idx :: Columnar f (Maybe Text),
        scanned :: Columnar f LocalTime
      }
  deriving (Generic, Beamable)

instance Table MetadataSourceT where
  data PrimaryKey MetadataSourceT f
    = MetadataSourceKey (Columnar f UUID)
    deriving (Generic, Beamable)

  primaryKey = MetadataSourceKey . view #id

type MetadataSource = MetadataSourceT Identity

type MetadataSourceKey = PrimaryKey MetadataSourceT Identity

instance Hashable MetadataSourceKey

deriving instance Show MetadataSource

deriving instance Eq MetadataSource

deriving instance (Show (Columnar f UUID)) => Show (PrimaryKey MetadataSourceT (f :: Type -> Type))

deriving instance (Eq (Columnar f UUID)) => Eq (PrimaryKey MetadataSourceT (f :: Type -> Type))

data ArtistGenreT (f :: Type -> Type)
  = ArtistGenre
      { artist_id :: PrimaryKey ArtistT f,
        genre_id :: PrimaryKey GenreT f
      }
  deriving (Generic, Beamable)

instance Table ArtistGenreT where
  data PrimaryKey ArtistGenreT f
    = ArtistGenreKey (PrimaryKey ArtistT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)

  primaryKey = ArtistGenreKey <$> view #artist_id <*> view #genre_id

data AlbumGenreT (f :: Type -> Type)
  = AlbumGenre
      { album_id :: PrimaryKey AlbumT f,
        genre_id :: PrimaryKey GenreT f
      }
  deriving (Generic, Beamable)

instance Table AlbumGenreT where
  data PrimaryKey AlbumGenreT f
    = AlbumGenreKey (PrimaryKey AlbumT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)

  primaryKey = AlbumGenreKey <$> view #album_id <*> view #genre_id

data TrackArtistAliasT (f :: Type -> Type)
  = TrackArtistAlias
      { track_id :: PrimaryKey TrackT f,
        artist_id :: PrimaryKey ArtistT f
      }
  deriving (Generic, Beamable)

instance Table TrackArtistAliasT where
  data PrimaryKey TrackArtistAliasT f
    = TrackArtistAliasKey (PrimaryKey TrackT f) (PrimaryKey ArtistT f)
    deriving (Generic, Beamable)

  primaryKey = TrackArtistAliasKey <$> view #track_id <*> view #artist_id

data TrackGenreT (f :: Type -> Type)
  = TrackGenre
      { track_id :: PrimaryKey TrackT f,
        genre_id :: PrimaryKey GenreT f
      }
  deriving (Generic, Beamable)

instance Table TrackGenreT where
  data PrimaryKey TrackGenreT f
    = TrackGenreKey (PrimaryKey TrackT f) (PrimaryKey GenreT f)
    deriving (Generic, Beamable)

  primaryKey = TrackGenreKey <$> view #track_id <*> view #genre_id

newtype Interval = Interval NominalDiffTime
  deriving (Show, Eq)

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
