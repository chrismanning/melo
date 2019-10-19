{-# LANGUAGE DerivingStrategies #-}

module Melo.API where

import GHC.Generics
import Melo.API.Haxl
import Melo.GraphQL.Resolve
import Melo.Library.API

data Query m
  = Query
      { library :: FieldResolver m (QLFieldContext (Query m)) (Library m)
      }
  deriving (Generic)

instance GraphQlType (Query Haxl) where
  resolver = Query
    { library = \_ _args fields -> resolveFields resolver LibraryFieldContext fields
    }

instance QLFieldResolve Haxl (Query Haxl) where
  data QLFieldContext (Query Haxl) = QueryFieldContext
