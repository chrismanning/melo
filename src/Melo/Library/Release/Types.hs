{-# LANGUAGE DeriveAnyClass #-}

module Melo.Library.Release.Types where

import Hasql.Decoders qualified as Hasql
import Data.Aeson qualified as A
import Data.Attoparsec.ByteString.Char8 qualified as P
import Data.Hashable
import Data.Time
import Data.UUID
import Melo.Database.Repo
import Melo.Library.Artist.Name.Types
import Melo.Lookup.MusicBrainz qualified as MB
import Opaleye.Internal.HaskellDB.PrimQuery (Literal (..), PrimExpr (..))
import Rel8

data ReleaseTable f = ReleaseTable
  { id :: Column f ReleaseRef,
    title :: Column f Text,
    comment :: Column f (Maybe Text),
    year_released :: Column f (Maybe Text),
    length :: Column f (Maybe CalendarDiffTime),
    musicbrainz_id :: Column f (Maybe Text),
    original_year_released :: Column f (Maybe Text),
    musicbrainz_group_id :: Column f (Maybe Text),
    catalogue_number :: Column f (Maybe Text),
    kind :: Column f ReleaseKind
  }
  deriving (Generic, Rel8able)

type ReleaseEntity = ReleaseTable Result

deriving instance Show ReleaseEntity
deriving via (FromGeneric ReleaseEntity) instance TextShow ReleaseEntity

deriving instance Eq ReleaseEntity

instance Entity ReleaseEntity where
  type NewEntity ReleaseEntity = NewRelease
  type PrimaryKey ReleaseEntity = ReleaseRef
  primaryKey e = e.id

newtype ReleaseRef = ReleaseRef UUID
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (DBType, DBEq, Hashable)
  deriving TextShow via FromGeneric ReleaseRef

instance From ReleaseRef UUID where
  from (ReleaseRef uuid) = uuid

data ReleaseKind
  = AlbumKind
  | SingleKind
  | EPKind
  | LiveKind
  | CompilationKind
  | OtherKind
  deriving (Show, Eq, Ord, Generic, Hashable)
  deriving TextShow via FromGeneric ReleaseKind
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripSuffix "Kind", ToLower]] ReleaseKind

instance DBType ReleaseKind where
  typeInformation =
    TypeInformation
      { encode,
        decode = Rel8.Decoder {
          binary,
          parser = P.parseOnly (parser' <* P.endOfInput),
          delimiter = ','
        },
        typeName = "text"
      }
    where
      encode AlbumKind = ConstExpr $ StringLit "album"
      encode SingleKind = ConstExpr $ StringLit "single"
      encode EPKind = ConstExpr $ StringLit "ep"
      encode LiveKind = ConstExpr $ StringLit "live"
      encode CompilationKind = ConstExpr $ StringLit "compilation"
      encode OtherKind = ConstExpr $ StringLit "other"
      binary :: Hasql.Value ReleaseKind
      binary = Hasql.text <&> \case
        "album" -> AlbumKind
        "single" -> SingleKind
        "ep" -> EPKind
        "live" -> LiveKind
        "compilation" -> CompilationKind
        _ -> OtherKind
      parser' = do
        P.takeByteString >>= \case
          "album" -> pure AlbumKind
          "single" -> pure SingleKind
          "ep" -> pure EPKind
          "live" -> pure LiveKind
          "compilation" -> pure CompilationKind
          _ -> pure OtherKind

instance DBEq ReleaseKind where

data Release = Release
  { ref :: ReleaseRef,
    title :: Text,
    yearReleased :: Maybe Text,
    originalYearReleased :: Maybe Text,
    catalogueNumber :: Maybe Text,
    artists :: [ArtistNameEntity],
    kind :: ReleaseKind
  }
  deriving (Show, Eq, Generic, Hashable)
  deriving TextShow via FromGeneric Release

mkRelease :: Foldable f => f ArtistNameEntity -> ReleaseEntity -> Release
mkRelease releaseArtists e =
  Release
    { ref = e.id,
      title = e.title,
      yearReleased = e.year_released,
      originalYearReleased = e.original_year_released,
      catalogueNumber = e.catalogue_number,
      artists = toList releaseArtists,
      kind = e.kind
    }

data NewRelease = NewRelease
  { title :: Text,
    comment :: Maybe Text,
    yearReleased :: Maybe Text,
    musicbrainzId :: Maybe MB.MusicBrainzId,
    originalYearReleased :: Maybe Text,
    musicbrainzGroupId :: Maybe MB.MusicBrainzId,
    catalogueNumber :: Maybe Text,
    kind :: ReleaseKind
  }
  deriving (Generic, Eq, Ord, Show)
  deriving TextShow via FromGeneric NewRelease

instance A.ToJSON NewRelease where
  toEncoding = A.genericToEncoding A.defaultOptions

fromMusicBrainz :: Maybe MB.ReleaseGroup -> Maybe MB.Release -> Maybe NewRelease
fromMusicBrainz Nothing Nothing = Nothing
fromMusicBrainz (Just releaseGroup) (Just release) =
  Just
    NewRelease
      { title = releaseGroup.title,
        yearReleased = release.date,
        originalYearReleased = releaseGroup.firstReleaseDate,
        musicbrainzId = Just release.id,
        musicbrainzGroupId = Just releaseGroup.id,
        catalogueNumber = release ^? #labelInfo . _Just . _head . #catalogNumber . _Just,
        kind = fromMaybe AlbumKind $ parsePrimaryType <$> releaseGroup.primaryType,
        comment = Nothing
      }
fromMusicBrainz (Just releaseGroup) Nothing =
  Just
    NewRelease
      { title = releaseGroup.title,
        yearReleased = Nothing,
        originalYearReleased = releaseGroup.firstReleaseDate,
        musicbrainzId = Nothing,
        musicbrainzGroupId = Just releaseGroup.id,
        catalogueNumber = Nothing,
        kind = fromMaybe AlbumKind $ parsePrimaryType <$> releaseGroup.primaryType,
        comment = Nothing
      }
fromMusicBrainz Nothing (Just release) =
  Just
    NewRelease
      { title = release.title,
        yearReleased = release.date,
        originalYearReleased = Nothing,
        musicbrainzId = Just release.id,
        musicbrainzGroupId = Nothing,
        catalogueNumber = release ^? #labelInfo . _Just . _head . #catalogNumber . _Just,
        kind = fromMaybe AlbumKind $ parsePrimaryType <$> release.primaryType,
        comment = Nothing
      }

parsePrimaryType :: Text -> ReleaseKind
parsePrimaryType = \case
  "Album" -> AlbumKind
  "Single" -> SingleKind
  "EP" -> EPKind
  _ -> OtherKind

instance From NewRelease (ReleaseTable Expr) where
  from a =
    ReleaseTable
      { id = function "uuid_generate_v4" (),
        title = lit a.title,
        comment = lit Nothing,
        year_released = lit a.yearReleased,
        original_year_released = lit a.originalYearReleased,
        length = lit Nothing,
        musicbrainz_id = lit $ MB.mbid <$> a.musicbrainzId,
        musicbrainz_group_id = lit $ MB.mbid <$> a.musicbrainzGroupId,
        kind = lit a.kind,
        catalogue_number = lit a.catalogueNumber
      }

fromNewRelease :: NewRelease -> UUID -> ReleaseEntity
fromNewRelease r ref =
  ReleaseTable
    { id = ReleaseRef ref,
      title = r.title,
      comment = Nothing,
      year_released = r.yearReleased,
      original_year_released = r.originalYearReleased,
      length = Nothing,
      musicbrainz_id = MB.mbid <$> r.musicbrainzId,
      musicbrainz_group_id = MB.mbid <$> r.musicbrainzGroupId,
      kind = r.kind,
      catalogue_number = r.catalogueNumber
    }
