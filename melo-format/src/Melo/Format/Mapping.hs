{-# LANGUAGE StrictData #-}

module Melo.Format.Mapping where

import Data.Default
import Data.List.NonEmpty
import Data.Text
import GHC.Generics

data FieldMappings = FieldMappings
  { ape :: Maybe FieldMapping,
    id3v1 :: Maybe FieldMapping,
    id3v2_3 :: Maybe FieldMapping,
    id3v2_4 :: Maybe FieldMapping,
    riff :: Maybe FieldMapping,
    vorbis :: Maybe FieldMapping,
    cue :: Maybe FieldMapping
  }
  deriving (Show, Eq, Generic)

instance Default FieldMappings where
  def =
    FieldMappings
      { ape = Nothing,
        id3v1 = Nothing,
        id3v2_3 = Nothing,
        id3v2_4 = Nothing,
        riff = Nothing,
        vorbis = Nothing,
        cue = Nothing
      }

data FieldMapping
  = FieldMapping
      { canonicalForm :: Text,
        fieldMatcher :: FieldMatchMode
      }
    deriving (Show, Eq, Generic)

type FieldMappingSelector = (FieldMappings -> Maybe FieldMapping)

data FieldMatchMode = CaseSensitiveMapping | CaseInsensitiveMapping
  deriving (Show, Eq, Generic)

fieldMatches :: FieldMapping -> Text -> Bool
fieldMatches FieldMapping{canonicalForm=k, fieldMatcher=CaseSensitiveMapping} k' = k == k'
fieldMatches FieldMapping{canonicalForm=k, fieldMatcher=CaseInsensitiveMapping} k' = toLower k == toLower k'

caseSensitiveMapping :: Text -> Maybe FieldMapping
caseSensitiveMapping m = Just $ FieldMapping m CaseSensitiveMapping

caseInsensitiveMapping :: Text -> Maybe FieldMapping
caseInsensitiveMapping m = Just $ FieldMapping m CaseInsensitiveMapping

newtype TagMapping = TagMapping (NonEmpty FieldMappings)
  deriving (Show, Eq, Generic)

singletonTagMapping :: FieldMappings -> TagMapping
singletonTagMapping m = TagMapping (m :| [])

headTagMapping :: TagMapping -> FieldMappings
headTagMapping (TagMapping (m :| _)) = m

instance Semigroup TagMapping where
  (TagMapping xs) <> (TagMapping ys) = TagMapping $ xs <> ys

albumArtist :: TagMapping
albumArtist = albumArtistTag <> trackArtistTag <> composerTag <> performerTag

artist :: TagMapping
artist = trackArtistTag <> albumArtistTag <> composerTag <> performerTag

album :: TagMapping
album = albumTitleTag

trackTitle :: TagMapping
trackTitle = trackTitleTag

trackNumber :: TagMapping
trackNumber = trackNumberTag

year :: TagMapping
year = yearTag <> originalReleaseYearTag

originalReleaseYear :: TagMapping
originalReleaseYear = originalReleaseYearTag <> yearTag

genre :: TagMapping
genre = genreTag

totalTracks :: TagMapping
totalTracks = totalTracksTag <> trackTotalTag

totalDiscs :: TagMapping
totalDiscs = totalDiscsTag <> discTotalTag

albumTitleTag :: TagMapping
albumTitleTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Album",
        id3v1 = caseSensitiveMapping "TALB",
        id3v2_3 = caseSensitiveMapping "TALB",
        id3v2_4 = caseSensitiveMapping "TALB",
        riff = caseSensitiveMapping "IPRD",
        vorbis = caseInsensitiveMapping "ALBUM",
        cue = caseSensitiveMapping "ALBUM_TITLE"
      }

albumTitleSortTag :: TagMapping
albumTitleSortTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "ALBUMSORT",
        id3v2_3 = caseSensitiveMapping "TSOA",
        id3v2_4 = caseSensitiveMapping "TSOA",
        vorbis = caseInsensitiveMapping "ALBUMSORT"
      }

trackTitleTag :: TagMapping
trackTitleTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Title",
        id3v1 = caseSensitiveMapping "TIT2",
        id3v2_3 = caseSensitiveMapping "TIT2",
        id3v2_4 = caseSensitiveMapping "TIT2",
        riff = caseSensitiveMapping "INAME",
        vorbis = caseInsensitiveMapping "TITLE",
        cue = caseSensitiveMapping "TITLE"
      }

trackTitleSortTag :: TagMapping
trackTitleSortTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "TITLESORT",
        id3v2_3 = caseSensitiveMapping "TSOT",
        id3v2_4 = caseSensitiveMapping "TSOT",
        vorbis = caseInsensitiveMapping "TITLESORT"
      }

albumArtistTag :: TagMapping
albumArtistTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Album Artist",
        id3v2_3 = caseSensitiveMapping "TPE2",
        id3v2_4 = caseSensitiveMapping "TPE2",
        vorbis = caseInsensitiveMapping "ALBUMARTIST",
        cue = caseSensitiveMapping "ALBUM_PERFORMER"
      }

albumArtistSortTag :: TagMapping
albumArtistSortTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "ALBUMARTISTSORT",
        id3v2_3 = caseSensitiveMapping "TSO2",
        vorbis = caseInsensitiveMapping "ALBUMARTISTSORT"
      }

trackArtistTag :: TagMapping
trackArtistTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Artist",
        id3v1 = caseSensitiveMapping "TPE1",
        id3v2_3 = caseSensitiveMapping "TPE1",
        id3v2_4 = caseSensitiveMapping "TPE1",
        riff = caseSensitiveMapping "IART",
        vorbis = caseInsensitiveMapping "ARTIST",
        cue = caseSensitiveMapping "PERFORMER"
      }

trackArtistSortTag :: TagMapping
trackArtistSortTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "ARTISTSORT",
        id3v2_3 = caseSensitiveMapping "TSOP",
        id3v2_4 = caseSensitiveMapping "TSOP",
        vorbis = caseInsensitiveMapping "ARTISTSORT"
      }

composerTag :: TagMapping
composerTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Composer",
        id3v2_3 = caseSensitiveMapping "TCOM",
        id3v2_4 = caseSensitiveMapping "TCOM",
        riff = caseSensitiveMapping "IMUS",
        vorbis = caseInsensitiveMapping "COMPOSER"
      }

performerTag :: TagMapping
performerTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Performer",
        id3v2_3 = caseSensitiveMapping "IPLS",
        id3v2_4 = caseSensitiveMapping "TMCL",
        vorbis = caseInsensitiveMapping "PERFORMER",
        cue = caseSensitiveMapping "PERFORMER"
      }

yearTag :: TagMapping
yearTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Year",
        id3v1 = caseSensitiveMapping "TYER",
        id3v2_3 = caseSensitiveMapping "TYER",
        id3v2_4 = caseSensitiveMapping "TDRC",
        vorbis = caseInsensitiveMapping "DATE",
        cue = caseSensitiveMapping "DATE"
      }

originalReleaseYearTag :: TagMapping
originalReleaseYearTag =
  singletonTagMapping
    def
      { id3v2_3 = caseSensitiveMapping "TORY",
        id3v2_4 = caseSensitiveMapping "TDOR",
        vorbis = caseInsensitiveMapping "ORIGINALDATE"
      }

trackNumberTag :: TagMapping
trackNumberTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Track",
        id3v1 = caseSensitiveMapping "TRCK",
        id3v2_3 = caseSensitiveMapping "TRCK",
        id3v2_4 = caseSensitiveMapping "TRCK",
        vorbis = caseInsensitiveMapping "TRACKNUMBER",
        cue = caseSensitiveMapping "TRACKNUMBER"
      }

totalTracksTag :: TagMapping
totalTracksTag =
  singletonTagMapping
    def
      { vorbis = caseInsensitiveMapping "TOTALTRACKS",
        cue = caseSensitiveMapping "TOTAL_TRACKS"
      }

trackTotalTag :: TagMapping
trackTotalTag =
  singletonTagMapping
    def
      { vorbis = caseInsensitiveMapping "TRACKTOTAL"
      }

genreTag :: TagMapping
genreTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Genre",
        id3v1 = caseSensitiveMapping "TCON",
        id3v2_3 = caseSensitiveMapping "TCON",
        id3v2_4 = caseSensitiveMapping "TCON",
        vorbis = caseInsensitiveMapping "GENRE",
        cue = caseSensitiveMapping "GENRE"
      }

commentTag :: TagMapping
commentTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Comment",
        id3v1 = caseSensitiveMapping "COMM",
        id3v2_3 = caseSensitiveMapping "COMM",
        id3v2_4 = caseSensitiveMapping "COMM",
        vorbis = caseInsensitiveMapping "COMMENT"
      }

discNumberTag :: TagMapping
discNumberTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Disc",
        id3v2_3 = caseSensitiveMapping "TPOS",
        id3v2_4 = caseSensitiveMapping "TPOS",
        vorbis = caseInsensitiveMapping "DISCNUMBER"
      }

totalDiscsTag :: TagMapping
totalDiscsTag =
  singletonTagMapping
    def
      { vorbis = caseInsensitiveMapping "TOTALDISCS"
      }

discTotalTag :: TagMapping
discTotalTag =
  singletonTagMapping
    def
      { vorbis = caseInsensitiveMapping "DISCTOTAL"
      }

encodedBy :: TagMapping
encodedBy =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "EncodedBy",
        id3v2_3 = caseSensitiveMapping "TENC",
        id3v2_4 = caseSensitiveMapping "TENC",
        riff = caseSensitiveMapping "IENC",
        vorbis = caseInsensitiveMapping "ENCODEDBY"
      }

language :: TagMapping
language =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Language",
        id3v2_3 = caseSensitiveMapping "TLAN",
        id3v2_4 = caseSensitiveMapping "TLAN",
        riff = caseSensitiveMapping "ILNG",
        vorbis = caseInsensitiveMapping "LANGUAGE"
      }
