module Melo.Lookup.Covers where

import Control.Concurrent.Classy
import Control.Concurrent.TokenLimiter
import Control.Exception.Safe
import Control.Lens hiding (from, lens)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson as A
import Data.Aeson.Casing (trainCase)
import Data.Aeson.Types
import Data.Char
import Data.Default
import Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Vector (Vector ())
import Data.Vector qualified as V
import GHC.Generics (Generic)
import GHC.Records
import Melo.Common.Http
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Format.Mapping
  ( FieldMappings (..),
    TagMapping (..),
    caseInsensitiveMapping,
    singletonTagMapping,
  )
import Melo.Format.Mapping qualified as M
import Melo.Format.Metadata qualified as F
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.URI
import Network.Wreq qualified as Wr
import Network.Wreq.Session qualified as WrS
import Streaming as S
import Streaming.Prelude as S

requestUri :: Text
requestUri = "https://covers.musichoarders.xyz/api/search"

data SearchRequest = SearchRequest
  { artist :: Text,
    album :: Text,
    country :: Text,
    sources :: NonEmpty Source
  }
  deriving (Generic)

instance ToJSON SearchRequest where
  toEncoding = genericToEncoding defaultOptions

data Source
  = Bandcamp
  | Qobuz
  | Tidal

instance ToJSON Source where
  toJSON Bandcamp = toJSON @String "bandcamp"
  toJSON Qobuz = toJSON @String "qobuz"
  toJSON Tidal = toJSON @String "tidal"
  toEncoding Bandcamp = toEncoding @String "bandcamp"
  toEncoding Qobuz = toEncoding @String "qobuz"
  toEncoding Tidal = toEncoding @String "tidal"

instance FromJSON Source where
  parseJSON (A.String "bandcamp") = pure Bandcamp
  parseJSON (A.String "qobuz") = pure Qobuz
  parseJSON (A.String "tidal") = pure Tidal
  parseJSON invalid =
      prependFailure "parsing Source failed, "
          (typeMismatch "String" invalid)

data SearchResult
  = SourceResult
  | DoneResult
  | CoverResult
      { smallCoverUrl :: Text,
        bigCoverUrl :: Text,
        source :: Source
      }
  deriving Generic

instance FromJSON SearchResult where
  parseJSON =
    genericParseJSON
      defaultOptions
        { sumEncoding =
            TaggedObject
              { tagFieldName = "type",
                contentsFieldName = ""
              }
        }

class CoverService m where
--  searchForCovers :: Album -> m 
