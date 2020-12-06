module Melo.GraphQL.Where where

import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text
import GHC.Generics

data Where
  = WhereEqExpr EqExpr
  | WhereNotEqExpr NotEqExpr
  | WhereContainsExpr ContainsExpr
  | WhereInExpr InExpr
  deriving (Generic)

instance GQLType Where where
  type KIND Where = INPUT

newtype EqExpr = EqExpr
  { eq :: Text
  }
  deriving (Generic)

instance GQLType EqExpr where
  type KIND EqExpr = INPUT

newtype NotEqExpr = NotEqExpr
  { notEq :: Text
  }
  deriving (Generic)

instance GQLType NotEqExpr where
  type KIND NotEqExpr = INPUT

newtype ContainsExpr = ContainsExpr
  { contains :: Text
  }
  deriving (Generic)

instance GQLType ContainsExpr where
  type KIND ContainsExpr = INPUT

newtype InExpr = InExpr
  { in' :: [Text]
  }
  deriving (Generic)

instance GQLType InExpr where
  type KIND InExpr = INPUT
