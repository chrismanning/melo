{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Melo.GraphQL.Introspect where

import Data.Generic.HKD
import Data.List.NonEmpty hiding (filter)
import Data.Proxy
import Data.Singletons
import Data.Singletons.TH
import Data.Tagged
import Data.Text as T hiding (filter)
import Data.Vector hiding (filter)
import GHC.Generics
import GHC.TypeLits as TL

import Debug.Trace

data GQLTypeKind = ScalarKind |
  ObjectKind |
  InterfaceKind |
  UnionKind |
  EnumKind |
  InputObjectKind |
  ListKind |
  NonNullKind
  deriving (Show, Eq, Generic, Enum)

$(genSingletons [''GQLTypeKind])

data GQLSchema = GQLSchema {
  types :: NonEmpty GQLType,
  queryType :: GQLType,
  mutationType :: Maybe GQLType,
  subscriptionType :: Maybe GQLType
} deriving (Generic)

data FieldsArgs = FieldsArgs {
  includeDeprecated :: Bool
} deriving (Generic)

data EnumValuesArgs = EnumValuesArgs {
  includeDeprecated :: Bool
} deriving (Generic)

data GQLType = GQLType {
  kind :: GQLTypeKind,
  name :: Maybe Text,
  description :: Maybe Text,
  fields :: FieldsArgs -> Maybe [GQLField],
  interfaces :: Maybe [GQLType],
  possibleTypes :: Maybe [GQLType],
  enumValues :: EnumValuesArgs -> Maybe [GQLEnumValue],
  inputFields :: Maybe [GQLInputValue],
  ofType :: Maybe GQLType
} deriving (Generic)

data GQLField = GQLField {
  name :: Text,
  description :: Maybe Text,
  args :: [GQLInputValue],
  fieldType :: GQLType,
  isDeprecated :: Bool,
  deprecationReason :: Maybe Text
} deriving (Generic)

data GQLInputValue = GQLInputValue {
  name :: Text,
  description :: Maybe Text,
  inputValueType :: GQLType,
  defaultValue :: Maybe Text
} deriving (Generic)

data GQLEnumValue = GQLEnumValue {
  name :: Text,
  description :: Maybe Text,
  isDeprecated :: Bool,
  deprecationReason :: Maybe Text
} deriving (Show, Eq, Generic)

class GraphQLType a where
  type TypeKind a :: GQLTypeKind
  asGQLType :: GQLType
  default asGQLType :: GQLType
  asGQLType = GQLType {
    kind = typeKind @a,
    name = Just $ typeName @a,
    description = description @a,
    fields = \(FieldsArgs inclDep) -> case typeKind @a of
      ObjectKind -> typeFields @a inclDep
      InterfaceKind -> typeFields @a inclDep
      _ -> Nothing,
    interfaces = interfaces @a,
    possibleTypes = possibleTypes @a,
    enumValues = \(EnumValuesArgs inclDep) -> typeEnumValues @a inclDep,
    inputFields = typeInputFields @a,
    ofType = ofType @a
  }

  typeKind :: GQLTypeKind
  default typeKind :: SingI (TypeKind a) => GQLTypeKind
  typeKind = fromSing $ sing @(TypeKind a)

  typeName :: Text
  default typeName :: GTypeName (Rep a) => Text
  typeName = typeName' @(Rep a)

  description :: Maybe Text
  default description :: Maybe Text
  description = Nothing

  typeFields :: Bool -> Maybe [GQLField]
  default typeFields :: GGraphQLFields a (KindHasFields (TypeKind a)) => Bool -> Maybe [GQLField]
  typeFields = typeFields' @a @(KindHasFields (TypeKind a))

  interfaces :: Maybe [GQLType]
  default interfaces :: Maybe [GQLType]
  interfaces = Nothing

  possibleTypes :: Maybe [GQLType]
  default possibleTypes :: Maybe [GQLType]
  possibleTypes = Nothing

  typeEnumValues :: Bool -> Maybe [GQLEnumValue]
  default typeEnumValues :: GGraphQLEnumValue a (KindHasEnumValues (TypeKind a)) => Bool -> Maybe [GQLEnumValue]
  typeEnumValues = typeEnumValues' @a @(KindHasEnumValues (TypeKind a))

  typeInputFields :: Maybe [GQLInputValue]
  default typeInputFields :: GGraphQLInputFields a (KindHasInputFields (TypeKind a)) => Maybe [GQLInputValue]
  typeInputFields = typeInputFields' @a @(KindHasInputFields (TypeKind a))

  ofType :: Maybe GQLType
  default ofType :: Maybe GQLType
  ofType = Nothing

data DatatypeProxy d (f :: * -> *) a = DatatypeProxy
type DatatypeProxy' d = DatatypeProxy d Proxy ()

class GTypeName f where
  typeName' :: Text

instance (Datatype d) => GTypeName (D1 d f) where
  typeName' = T.pack $ datatypeName (DatatypeProxy :: DatatypeProxy' d)

-- KINDS ----------------------------------

type family DataTypeName d where
  DataTypeName ('MetaData tn _ _ _) = tn

type family GGraphQLKind (a :: k -> *) :: GQLTypeKind
type instance GGraphQLKind (D1 d V1) =
    TypeError ('Text "Data type " ':<>: 'Text (DataTypeName d)
      ':<>: 'Text " has no constructors"
      ':$$: 'Text "Not a valid GraphQL type")
-- single constructor Enum
type instance GGraphQLKind (D1 d (C1 ('MetaCons _ _ 'False) U1)) = 'EnumKind
-- multiple constructor Enum
type instance GGraphQLKind (D1 d ((C1 c U1) :+: g)) = EqOrE 'EnumKind (GGraphQLKind (D1 d g))
    (TypeError ('Text "Data type " ':<>: 'Text (DataTypeName d)
      ':<>: 'Text " detected as GraphQL ENUM but has invalid constructor(s)"))
-- Object
type instance GGraphQLKind (D1 d (C1 ('MetaCons _ _ 'True) _)) = 'ObjectKind
-- single variant Union
type instance GGraphQLKind (D1 d (C1 ('MetaCons _ _ 'False) (S1 _ _))) = 'UnionKind
-- multiple variant Union
type instance GGraphQLKind (D1 d ((C1 ('MetaCons _ _ 'False) (S1 _ _)) :+: g)) = 'UnionKind
-- multiple unnamed fields
type instance GGraphQLKind (D1 d (C1 ('MetaCons _ _ 'False) ((S1 _ _) :*: g))) =
    TypeError ('Text "Data type " ':<>: 'Text " has multiple unnamed fields"
      ':$$: 'Text "Not a valid GraphQL type")

type family EqOrE a b e where
  EqOrE a a _ = a
  EqOrE a b e = e

-- FIELDS ---------------------------------

type family KindHasFields (k :: GQLTypeKind) :: Bool where
  KindHasFields 'ObjectKind = 'True
  KindHasFields 'InterfaceKind = 'True
  KindHasFields _ = 'False

class GGraphQLFields f (k :: Bool) where
  typeFields' :: Bool -> Maybe [GQLField]

instance GGraphQLFields' (Rep f) => GGraphQLFields f 'True where
  typeFields' = Just . typeObjectFields' @(Rep f)

instance GGraphQLFields a 'False where
  typeFields' = const Nothing

type family FieldResult a where
  FieldResult (Tagged _ a) = FieldResult a
  FieldResult (a -> b -> c) = TypeError
    ('TL.Text "GraphQL method fields may only have a single argument"
      ':<>: 'TL.Text "This argument must be a record defining named query arguments")
  FieldResult (a -> b) = FieldResult b
  FieldResult a = a

data DeprecatedTag
type Deprecated a = Tagged DeprecatedTag a

type family IsFieldDeprecated a :: Bool where
  IsFieldDeprecated (Tagged DeprecatedTag a) = 'True
  IsFieldDeprecated (Tagged x a) = IsFieldDeprecated a
  IsFieldDeprecated a = 'False

data SelectorProxy s (f :: * -> *) a = SelectorProxy
type SelectorProxy' s = SelectorProxy s Proxy ()

class GGraphQLFields' f where
  typeObjectFields' :: Bool -> [GQLField]

instance GGraphQLFields' f => GGraphQLFields' (M1 D p (M1 C c f)) where
  typeObjectFields' = typeObjectFields' @f

instance GGraphQLFields' f => GGraphQLFields' (M1 C c f) where
  typeObjectFields' = typeObjectFields' @f

instance (Selector s, GraphQLType (FieldResult a), SingI (IsFieldDeprecated a)) =>
  GGraphQLFields' (M1 S s (Rec0 a)) where
  typeObjectFields' inclDep = let f = GQLField {
    name = T.pack $ selName (SelectorProxy :: SelectorProxy' s),
    fieldType = asGQLType @(FieldResult a),
    description = Nothing,
    args = [],
    isDeprecated = fromSing $ sing @(IsFieldDeprecated a),
    deprecationReason = Nothing
  } in if not inclDep && (fromSing $ sing @(IsFieldDeprecated a)) then [] else [f]

instance (GGraphQLFields' f, GGraphQLFields' g) => GGraphQLFields' (f :*: g) where
  typeObjectFields' inclDep = typeObjectFields' @f inclDep <> typeObjectFields' @g inclDep

-- INPUT FIELDS ---------------------------------

type family KindHasInputFields (k :: GQLTypeKind) :: Bool where
  KindHasInputFields 'InputObjectKind = 'True
  KindHasInputFields _ = 'False

class GGraphQLInputFields f (b :: Bool) where
  typeInputFields' :: Maybe [GQLInputValue]

instance GGraphQLInputFields' (Rep f) => GGraphQLInputFields f 'True where
  typeInputFields' = Just $ inputFields' @(Rep f)

instance GGraphQLInputFields f 'False where
  typeInputFields' :: Maybe [GQLInputValue]
  typeInputFields' = Nothing

class GGraphQLInputFields' f where
  inputFields' :: [GQLInputValue]

instance GGraphQLInputFields' f => GGraphQLInputFields' (M1 D p f) where
  inputFields' = inputFields' @f

instance (Selector s, GraphQLType (FieldResult a)) => GGraphQLInputFields' (M1 S s (Rec0 a)) where
  inputFields' = let f = GQLInputValue {
    name = T.pack $ selName (SelectorProxy :: SelectorProxy' s),
    description = Nothing,
    inputValueType = asGQLType @(FieldResult a),
    defaultValue = Nothing
  } in [f]

instance (GGraphQLInputFields' f, GGraphQLInputFields' g) => GGraphQLInputFields' (f :*: g) where
  inputFields' = inputFields' @f <> inputFields' @g

instance (GGraphQLInputFields' f) => GGraphQLInputFields' (M1 C ('MetaCons x y 'True) f) where
  inputFields' = inputFields' @f

-- ENUM VALUES ---------------------------------

type family KindHasEnumValues (k :: GQLTypeKind) where
  KindHasEnumValues 'EnumKind = 'True
  KindHasEnumValues _ = 'False

class GGraphQLEnumValue f (b :: Bool) where
  typeEnumValues' :: Bool -> Maybe [GQLEnumValue]

instance GGraphQLEnumValue f 'False where
  typeEnumValues' = const Nothing

instance GGraphQLEnumValue' (Rep f) => GGraphQLEnumValue f 'True where
  typeEnumValues' = Just . enumValues' @(Rep f)

class GGraphQLEnumValue' f where
  enumValues' :: Bool -> [GQLEnumValue]

instance GGraphQLEnumValue' f => GGraphQLEnumValue' (M1 D p f) where
  enumValues' = enumValues' @f

instance Constructor c => GGraphQLEnumValue' (M1 C c U1) where
  enumValues' inclDep = let v = GQLEnumValue {
    name = T.pack $ conName (ConstructorProxy :: ConstructorProxy' c),
    description = Nothing,
    isDeprecated = False,
    deprecationReason = Nothing
  } in [v]

instance (GGraphQLEnumValue' f, GGraphQLEnumValue' g) => GGraphQLEnumValue' (f :+: g) where
  enumValues' inclDep = enumValues' @f inclDep <> enumValues' @g inclDep

data ConstructorProxy c (f :: * -> *) a = ConstructorProxy
type ConstructorProxy' c = ConstructorProxy c Proxy ()

-----------------------------------

data Schema q m s = Schema {
  query :: q,
  mutation :: m,
  schema :: s
}

instance GraphQLType Text where
  type TypeKind Text = 'ScalarKind
  typeKind = ScalarKind
  typeName = "String"
  description = Just "String type"

instance GraphQLType Int where
  type TypeKind Int = 'ScalarKind
  typeName = "Int"
  description = Just "Int type"

instance GraphQLType a => GraphQLType (Vector a) where
  type TypeKind (Vector a) = 'ListKind
  typeName = "List"
  description = fmap ("List of " <>) (description @a)
  ofType = Just $ asGQLType @a

instance GraphQLType a => GraphQLType [a] where
  type TypeKind [a] = 'ListKind
  typeName = "List"
  description = fmap ("List of " <>) (description @a)
  ofType = Just $ asGQLType @a

-----------------------

data IFace = IFace {
  a :: Text
} deriving (Generic)

instance GraphQLType IFace where
  type TypeKind IFace = 'InterfaceKind

data ExEnum = ExA | ExB
  deriving (Generic)

instance GraphQLType ExEnum where
  type TypeKind ExEnum = 'EnumKind

data ExUnion = ExInt Int | ExStr Text
  deriving (Generic)

instance GraphQLType ExUnion where
  type TypeKind ExUnion = 'UnionKind

data ExObj = ExObj {
  a :: Text,
  b :: BArgs -> Text,
  c :: ExEnum,
  d :: Deprecated Text
} deriving (Generic)

instance GraphQLType ExObj where
  type TypeKind ExObj = 'ObjectKind

data ExObj2 = ExObj2 {
  ref :: ExObj
} deriving (Generic)

instance GraphQLType ExObj2 where
  type TypeKind ExObj2 = 'ObjectKind

data BArgs = BArgs {
  i :: Int
} deriving (Generic)

instance GraphQLType BArgs where
  type TypeKind BArgs = 'InputObjectKind

-- type ExObjF f = HKD ExObj f

data ExG = ExG {
  a :: GQLField,
  b :: GQLField
}

data ExR m = ExR {
  a :: m Text,
  b :: BArgs -> m Text
}
