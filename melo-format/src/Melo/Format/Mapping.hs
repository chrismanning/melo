module Melo.Format.Mapping where

import Data.Default
import Data.Text

data FieldMappings
  = FieldMappings
      { ape :: FieldMapping,
        id3v1 :: FieldMapping,
        id3v2_3 :: FieldMapping,
        id3v2_4 :: FieldMapping,
        riff :: FieldMapping,
        vorbis :: FieldMapping
      }

instance Default FieldMappings where
  def =
    FieldMappings
      { ape = NoFieldMapping,
        id3v1 = NoFieldMapping,
        id3v2_3 = NoFieldMapping,
        id3v2_4 = NoFieldMapping,
        riff = NoFieldMapping,
        vorbis = NoFieldMapping
      }

data FieldMapping
  = FieldMapping
      { toCanonicalForm :: Text,
        fieldMatcher :: Text -> Bool
      }
  | NoFieldMapping

type FieldMappingSelector = (FieldMappings -> FieldMapping)

caseSensitiveMapping :: Text -> FieldMapping
caseSensitiveMapping m = FieldMapping m (== m)

caseInsensitiveMapping :: Text -> FieldMapping
caseInsensitiveMapping m = FieldMapping m (\x -> toLower x == toLower m)

newtype TagMapping = TagMapping [FieldMappings]

singletonTagMapping :: FieldMappings -> TagMapping
singletonTagMapping m = TagMapping [m]

headTagMapping :: TagMapping -> Maybe FieldMappings
headTagMapping (TagMapping [m]) = Just m
headTagMapping _ = Nothing

instance Monoid TagMapping where
  mempty = TagMapping []

  mappend = (<>)

instance Semigroup TagMapping where
  (TagMapping xs) <> (TagMapping ys) = TagMapping $ xs ++ ys

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
year = yearTag

genre :: TagMapping
genre = genreTag

albumTitleTag :: TagMapping
albumTitleTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Album",
        id3v1 = caseSensitiveMapping "TALB",
        id3v2_3 = caseSensitiveMapping "TALB",
        id3v2_4 = caseSensitiveMapping "TALB",
        riff = caseSensitiveMapping "IPRD",
        vorbis = caseInsensitiveMapping "ALBUM"
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
        vorbis = caseInsensitiveMapping "TITLE"
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
        vorbis = caseInsensitiveMapping "ALBUMARTIST"
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
        vorbis = caseInsensitiveMapping "ARTIST"
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
        vorbis = caseInsensitiveMapping "PERFORMER"
      }

yearTag :: TagMapping
yearTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Year",
        id3v1 = caseSensitiveMapping "TYER",
        id3v2_3 = caseSensitiveMapping "TYER",
        id3v2_4 = caseSensitiveMapping "TDRC",
        vorbis = caseInsensitiveMapping "DATE"
      }

trackNumberTag :: TagMapping
trackNumberTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Track",
        id3v1 = caseSensitiveMapping "TRCK",
        id3v2_3 = caseSensitiveMapping "TRCK",
        id3v2_4 = caseSensitiveMapping "TRCK",
        vorbis = caseInsensitiveMapping "TRACKNUMBER"
      }

genreTag :: TagMapping
genreTag =
  singletonTagMapping
    def
      { ape = caseInsensitiveMapping "Genre",
        id3v1 = caseSensitiveMapping "TCON",
        id3v2_3 = caseSensitiveMapping "TCON",
        id3v2_4 = caseSensitiveMapping "TCON",
        vorbis = caseInsensitiveMapping "GENRE"
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
