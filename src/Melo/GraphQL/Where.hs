{-# LANGUAGE DeriveAnyClass #-}

module Melo.GraphQL.Where where

import Data.Morpheus.Types

data Where
  = WhereEqExpr EqExpr
  | WhereNotEqExpr NotEqExpr
  | WhereContainsExpr ContainsExpr
  | WhereInExpr InExpr
  | WhereStartsWithExpr StartsWithExpr
  deriving (Generic, GQLType)

newtype EqExpr = EqExpr
  { eq :: Text
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

newtype NotEqExpr = NotEqExpr
  { notEq :: Text
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

newtype ContainsExpr = ContainsExpr
  { contains :: Text
  }
  deriving stock (Generic)
  deriving anyclass (GQLType)

newtype InExpr = InExpr
  { in' :: [Text]
  }
  deriving (Generic)
  deriving anyclass (GQLType)

newtype StartsWithExpr = StartsWithExpr
  { startsWith :: Text
  }
  deriving (Generic)
  deriving anyclass (GQLType)
