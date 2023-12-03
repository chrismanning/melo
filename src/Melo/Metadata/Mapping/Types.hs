{-# LANGUAGE DeriveAnyClass #-}

module Melo.Metadata.Mapping.Types where

import Data.Coerce
import Data.Default
import Data.List.NonEmpty as NE
import Data.Morpheus.Types as M
import Melo.Database.Repo
import Melo.Format qualified as F
import Rel8
  ( Column,
    Expr,
    JSONBEncoded (..),
    Rel8able (),
    Result,
    lit,
  )
import Melo.Library.Source.Cue

data TagMappingTable f = TagMappingTable
  { name :: Column f Text,
    field_mappings :: Column f (JSONBEncoded (NonEmpty FieldMapping))
  }
  deriving (Generic, Rel8able)

type TagMappingEntity = TagMappingTable Result

deriving instance Show TagMappingEntity

deriving via (FromGeneric TagMappingEntity) instance TextShow TagMappingEntity

instance HasField "tagMapping" TagMappingEntity TagMapping where
  getField e = TagMapping e.name $ fromJSONBEncoded e.field_mappings

data TagMapping = TagMapping
  { name :: Text,
    fieldMappings :: NonEmpty FieldMapping
  }
  deriving (Show, Eq, Generic, GQLType)
  deriving (TextShow) via FromGeneric TagMapping
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions TagMapping

instance From TagMapping F.TagMapping where
  from tm = F.TagMapping $ fmap from tm.fieldMappings

data FieldMapping = FieldMapping
  { formatId :: Text,
    name :: Text,
    kind :: FieldMatchingKind
  }
  deriving (Show, Eq, Generic, GQLType)
  deriving (TextShow) via FromGeneric FieldMapping
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions FieldMapping

instance From FieldMapping F.FieldMappings where
  from fm = case F.MetadataId fm.formatId of
    mid | mid == F.apeV1Id -> def { F.ape = Just (from fm) }
        | mid == F.apeV2Id -> def { F.ape = Just (from fm) }
        | mid == F.id3v1Id -> def { F.id3v1 = Just (from fm) }
        | mid == F.id3v23Id -> def { F.id3v2_3 = Just (from fm) }
        | mid == F.id3v24Id -> def { F.id3v2_4 = Just (from fm) }
        | mid == F.riffId -> def { F.riff = Just (from fm) }
        | mid == F.vorbisCommentsId -> def { F.vorbis = Just (from fm) }
        | mid == cueId -> def { F.cue = Just (from fm) }
    _ -> def

instance From FieldMapping F.FieldMapping where
  from fm = F.FieldMapping fm.name (from fm.kind)

data FieldMatchingKind
  = CaseSensitive
  | CaseInsensitive
  | Virtual
  deriving (Show, Eq, Generic, GQLType)
  deriving (TextShow) via FromGeneric FieldMatchingKind
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions FieldMatchingKind

instance From F.FieldMatchMode FieldMatchingKind where
  from F.CaseSensitiveMapping = CaseSensitive
  from F.CaseInsensitiveMapping = CaseInsensitive
  from F.Virtual = Virtual

instance From FieldMatchingKind F.FieldMatchMode where
  from CaseSensitive = F.CaseSensitiveMapping
  from CaseInsensitive = F.CaseInsensitiveMapping
  from Virtual = F.Virtual

instance Entity (TagMappingTable Result) where
  type NewEntity (TagMappingTable Result) = TagMapping
  type PrimaryKey (TagMappingTable Result) = Text
  primaryKey e = e.name

fromTagMapping :: Text -> F.TagMapping -> TagMapping
fromTagMapping name (F.TagMapping fm) = TagMapping name (fm >>= \fm' ->
  NE.fromList $ catMaybes [
    FieldMapping (coerce F.apeV1Id) <$> fmap (.canonicalForm) fm'.ape <*> fmap (from . (.fieldMatcher)) fm'.ape,
    FieldMapping (coerce F.apeV2Id) <$> fmap (.canonicalForm) fm'.ape <*> fmap (from . (.fieldMatcher)) fm'.ape,
    FieldMapping (coerce F.id3v1Id) <$> fmap (.canonicalForm) fm'.id3v1 <*> fmap (from . (.fieldMatcher)) fm'.id3v1,
    FieldMapping (coerce F.id3v23Id) <$> fmap (.canonicalForm) fm'.id3v2_3 <*> fmap (from . (.fieldMatcher)) fm'.id3v2_3,
    FieldMapping (coerce F.id3v24Id) <$> fmap (.canonicalForm) fm'.id3v2_4 <*> fmap (from . (.fieldMatcher)) fm'.id3v2_4,
    FieldMapping (coerce F.riffId) <$> fmap (.canonicalForm) fm'.riff <*> fmap (from . (.fieldMatcher)) fm'.riff,
    FieldMapping (coerce F.vorbisCommentsId) <$> fmap (.canonicalForm) fm'.vorbis <*> fmap (from . (.fieldMatcher)) fm'.vorbis,
    FieldMapping (coerce cueId) <$> fmap (.canonicalForm) fm'.cue <*> fmap (from . (.fieldMatcher)) fm'.cue
  ])

instance From TagMapping (TagMappingTable Expr) where
  from c =
    TagMappingTable
      { name = lit c.name,
        field_mappings = lit (JSONBEncoded (from c.fieldMappings))
      }

instance From TagMapping (TagMappingTable Result) where
  from c =
    TagMappingTable
      { name = c.name,
        field_mappings = JSONBEncoded (from c.fieldMappings)
      }

type UpdateTagMapping = TagMappingTable Result
