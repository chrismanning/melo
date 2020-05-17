module Melo.Library.API where

import Control.Algebra
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson as A
import Data.Aeson.Encoding as A
import Data.Generic.HKD
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text as T hiding (null)
import Data.Traversable
import Data.UUID (toText)
import Database.Beam hiding (C)
import Debug.Trace
import GHC.OverloadedLabels ()
import Melo.Common.Haxl
import qualified Melo.Database.Model as DB
import Melo.Library.Source.API
import Melo.Library.Source.Repo

data Library m = Library
  { sources :: m [SearchResult Source]
  }
  deriving (Generic, GQLType)

resolveLibrary :: forall sig m e. (Has SourceRepository sig m) => ResolveQ e m Library
resolveLibrary = lift $ pure Library {
  sources = resolveSources @sig @m
}
