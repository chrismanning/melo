{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Source.API where

--import Melo.GraphQL.Resolve
--import Melo.GraphQL.Introspect

import Basement.From
import Control.Algebra
import Control.Effect.Lift
import Control.Exception.Safe (MonadThrow)
import Control.Lens hiding (from)
import Control.Monad
import Data.Aeson as A
import Data.ByteString (ByteString)
import Data.Functor
import Data.Generic.HKD
import Data.Kind (Type)
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text
import Data.Typeable
import Data.UUID (toText)
import GHC.Generics (Generic)
import Melo.Common.Haxl
import qualified Melo.Database.Model as DB
import qualified Melo.Format as F
import Melo.Library.Source.Repo
import qualified Melo.Library.Source.Types as T
import Network.URI
import Network.URI.JSON ()
import Database.Beam.Postgres (PgJSONB(..))

data Source = Source
  { id :: Text,
    metadata :: Metadata,
    sourceUri :: Text,
    downloadUri :: Text
  }
  deriving (Generic, GQLType, ToJSON)

type Score = Int

data SearchResult a = SearchResult
  { score :: Score,
    result :: a
  }
  deriving (Generic, GQLType)

deriving instance ToJSON a => ToJSON (SearchResult a)

resolveSources ::
  (Has SourceRepository sig m) =>
  Res e m [SearchResult Source]
resolveSources =
  lift $
    fmap (fmap (SearchResult 100 . from)) getAllSources

instance From DB.Source Source where
  from s =
    Source
      { id = toText (s ^. #id),
        metadata = from s,
        sourceUri = s ^. #source_uri,
        downloadUri = ""
      }

data Metadata = Metadata
  { apev1 :: Maybe [MetadataPair],
    apev2 :: Maybe [MetadataPair],
    id3v1 :: Maybe [MetadataPair],
    id3v23 :: Maybe [MetadataPair],
    id3v24 :: Maybe [MetadataPair],
    vorbis :: Maybe [MetadataPair]
  }
  deriving (Generic, ToJSON, GQLType)

data MetadataPair = MetadataPair
  { key :: Text,
    value :: Text
  }
  deriving (Generic, FromJSON, ToJSON, GQLType)

instance From DB.Source Metadata where
  from s = let m :: Maybe [MetadataPair] = toMaybe $ fromJSON (s ^. #metadata . fromPg)
               fmt = F.MetadataId (s ^. #metadata_format) in
    Metadata
      { apev1 = mfilter (const (fmt == F.apeV1Id)) m,
        apev2 = mfilter (const (fmt == F.apeV2Id)) m,
        id3v1 = mfilter (const (fmt == F.id3v1Id)) m,
        id3v23 = mfilter (const (fmt == F.id3v23Id)) m,
        id3v24 = mfilter (const (fmt == F.id3v24Id)) m,
        vorbis = mfilter (const (fmt == F.vorbisCommentsId)) m
      }

toMaybe :: Result a -> Maybe a
toMaybe = \case
  Success a -> Just a
  Error _ -> Nothing

fromPg = to fromPg'

fromPg' :: PgJSONB a -> a
fromPg' (PgJSONB a) = a
