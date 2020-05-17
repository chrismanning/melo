{-# LANGUAGE DerivingStrategies #-}

module Melo.API where

import Control.Algebra
import Control.Carrier.Reader
import Data.ByteString.Lazy.Char8
import Data.Generic.HKD
import Data.Morpheus
import Data.Morpheus.Document
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Typeable
import GHC.Generics
import Melo.Library.API
import Melo.Library.Source.Repo
import Database.PostgreSQL.Simple (Connection)
import Data.Pool

data Query m = Query {
  library :: m (Library m)
} deriving (Generic, GQLType)

rootResolver :: ResolverE sig m => GQLRootResolver m () Query Undefined Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {library = resolveLibrary}
    , mutationResolver = Undefined
    , subscriptionResolver = Undefined
    }

gqlApi :: forall sig m. (ResolverE sig m, Typeable m) => ByteString -> m ByteString
gqlApi = interpreter (rootResolver @sig @m)

gqlApiIO :: Pool Connection -> ByteString -> IO ByteString
gqlApiIO pool r =
  withResource pool $ \conn ->
    runResolverE conn (gqlApi r)

type ResolverE sig m = (Has SourceRepository sig m)

runResolverE conn = runReader conn
  . runSourceRepositoryIO
