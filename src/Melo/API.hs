{-# LANGUAGE DerivingStrategies #-}

module Melo.API where

import Data.Generic.HKD
import GHC.Generics
import Melo.GraphQL.Introspect
import Melo.GraphQL.Resolve
import Melo.Library.API
import Melo.Library.Repo.Haxl

data Query = Query
  { library :: Library
  }
  deriving (Generic)

instance GraphQLType Query where
  type TypeKind Query = 'ObjectKind

data QueryCtx = QueryCtx

instance ObjectResolver Haxl Query where
  type ResolverContext Query = QueryCtx

instance GenericResolver Haxl Query where
  genericResolver =
    let g = build @Query
     in g (Resolve $ \_ _ fs -> resolveFieldValues LibraryCtx fs)
