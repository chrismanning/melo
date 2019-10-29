{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Melo.GraphQL.Introspect where

import Data.Aeson
import Data.Generic.HKD
import Data.List.NonEmpty
import Data.Proxy
import Data.Text as T
import Data.Vector
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
  deriving (Show, Eq, Generic)

instance ToJSON GQLTypeKind where
  toJSON ScalarKind = String "SCALAR"
  toJSON ObjectKind = String "OBJECT"
  toJSON InterfaceKind = String "INTERFACE"
  toJSON UnionKind = String "UNION"
  toJSON EnumKind = String "ENUM"
  toJSON InputObjectKind = String "INPUT_OBJECT"
  toJSON ListKind = String "LIST"
  toJSON NonNullKind = String "NON_NULL"

data GQLSchema = GQLSchema {
  types :: NonEmpty GQLType,
  queryType :: GQLType,
  mutationType :: Maybe GQLType,
  subscriptionType :: Maybe GQLType
} deriving (Show, Eq, Generic, ToJSON)

data GQLType = GQLType {
  kind :: GQLTypeKind,
  name :: Maybe Text,
  description :: Maybe Text,
  fields :: Maybe [GQLField],
  interfaces :: Maybe [GQLType],
  possibleTypes :: Maybe [GQLType],
  enumValues :: Maybe [GQLEnumValue],
  inputFields :: Maybe [GQLInputValue],
  ofType :: Maybe GQLType
} deriving (Show, Eq, Generic, ToJSON)

data GQLField = GQLField {
  name :: Text,
  description :: Maybe Text,
  args :: [GQLInputValue],
  fieldType :: GQLType,
  isDeprecated :: Bool,
  deprecationReason :: Maybe Text
} deriving (Show, Eq, Generic, ToJSON)

data GQLInputValue = GQLInputValue {
  name :: Text,
  description :: Maybe Text,
  inputValueType :: GQLType,
  defaultValue :: Maybe Text
} deriving (Show, Eq, Generic, ToJSON)

data GQLEnumValue = GQLEnumValue {
  name :: Text,
  description :: Maybe Text,
  isDeprecated :: Bool,
  deprecationReason :: Maybe Text
} deriving (Show, Eq, Generic, ToJSON)

class GraphQLType a where
  asGQLType :: GQLType
  default asGQLType :: GQLType
  asGQLType = GQLType {
    kind = typeKind @a,
    name = Just $ typeName @a,
    description = description @a,
    fields = typeFields @a True,
    interfaces = Nothing, --interfaces a,
    possibleTypes = Nothing, --possibleTypes a,
    enumValues = Nothing, --enumValues a True
    inputFields = Nothing, --inputFields a
    ofType = ofType @a
  }

  typeKind :: GQLTypeKind
  default typeKind :: (GGraphQLKind (Rep a)) => GQLTypeKind
  typeKind = typeKind' @(Rep a)

  typeName :: Text
  default typeName :: (GTypeName (Rep a)) => Text
  typeName = typeName' @(Rep a)

  description :: Maybe Text
  default description :: Maybe Text
  description = Nothing

  typeFields :: Bool -> Maybe [GQLField]
  default typeFields :: (GGraphQLFields (Rep a)) => Bool -> Maybe [GQLField]
  typeFields = typeFields' @(Rep a)

  interfaces :: Maybe [GQLType]
  default interfaces :: Maybe [GQLType]
  interfaces = Nothing

  possibleTypes :: Maybe [GQLType]
  default possibleTypes :: Maybe [GQLType]
  possibleTypes = Nothing

  enumValues :: Bool -> Maybe [GQLEnumValue]
  -- default enumValues :: (Generic a, GGraphQLEnumValue (Rep a)) => Bool -> Maybe [GQLEnumValue]
  -- enumValues = typeEnumValues' @(Rep a)

  inputFields :: Maybe [GQLInputValue]
  -- default inputFields :: (Generic a, GGraphQLInputValue (Rep a)) => Maybe [GQLInputValue]
  -- inputFields = typeInputFields' @(Rep a)

  ofType :: Maybe GQLType
  default ofType :: Maybe GQLType
  ofType = Nothing

data DatatypeProxy d (f :: * -> *) a = DatatypeProxy
type DatatypeProxy' d = DatatypeProxy d Proxy ()

class GTypeName f where
  typeName' :: Text

instance (Datatype d) => GTypeName (D1 d f) where
  typeName' = T.pack $ datatypeName (DatatypeProxy :: DatatypeProxy' d)

class GGraphQLEnumValue f where
  typeEnumValues' :: Bool -> Maybe [GQLEnumValue]

class GGraphQLInputValue f where
  typeInputFields' :: Maybe [GQLInputValue]

class GraphQLScalar a where

class GraphQLType a => GraphQLObject a where
  objectFields :: a -> Bool -> [GQLField]

class GGraphQLObject a where
  objectFields' :: f a -> [GQLField]

class GraphQLType a => GraphQLInterface a where
  interfaceFields :: a -> Bool -> [GQLField]

class GraphQLUnion a

class GraphQLEnum a

class GraphQLInputObject a where
  inputObjectFields :: a -> [GQLInputValue]

class GGraphQLInputFields a where
  inputObjectFields' :: f a -> [GQLInputValue]

class GraphQLWrappedType a where
  wrapperOfType :: a -> GQLType

-- instance GraphQLType a => GraphQLWrappedType [a] where
  -- wrapperOfType _ = asGQLType @a

-- KINDS ----------------------------------

type family GraphQLKind a :: k -> GQLTypeKind

class GGraphQLKind f where
  typeKind' :: GQLTypeKind

instance GGraphQLKind f => GGraphQLKind (M1 D p f) where
  typeKind' = typeKind' @f

instance (Constructor c) => GGraphQLKind (M1 C c f) where
  typeKind' = if conIsRecord (ConstructorProxy :: ConstructorProxy' c) then ObjectKind else UnionKind

instance GGraphQLKind ((M1 S s (Rec0 a)) :*: f) where
  typeKind' = ObjectKind

instance GGraphQLKind (M1 S s (Rec0 a)) where
  typeKind' = ObjectKind

instance GGraphQLKind (M1 C p U1) where
  typeKind' = EnumKind

instance GGraphQLKind ((M1 C p U1) :+: g) where
  typeKind' = EnumKind

instance GGraphQLKind ((M1 C p (M1 S s r)) :+: g) where
  typeKind' = UnionKind

-- FIELDS ---------------------------------

class GGraphQLFields f where
  typeFields' :: Bool -> Maybe [GQLField]

type family FieldResult a where
  FieldResult (a -> b -> c) = TypeError ('TL.Text "Fields functions may only have a single argument => a record to define named query arguments")
  FieldResult (a -> b) = FieldResult b
  FieldResult a = a

data SelectorProxy s (f :: * -> *) a = SelectorProxy
type SelectorProxy' s = SelectorProxy s Proxy ()

-- Object
instance (Selector s, GraphQLType (FieldResult a)) => GGraphQLFields (M1 S s (Rec0 a)) where
  typeFields' _ = let f = GQLField {
    name = T.pack $ selName (SelectorProxy :: SelectorProxy' s),
    fieldType = asGQLType @(FieldResult a),
    description = Nothing,
    args = [],
    isDeprecated = False,
    deprecationReason = Nothing
  } in
    Just [f]

-- Object
instance (f ~ (M1 S s (Rec0 a)), GraphQLType (FieldResult a), Selector s, GGraphQLFields g) => GGraphQLFields (f :*: g) where
  typeFields' inclDep = case typeFields' @f inclDep of
    Just fs -> (fs <>) <$> typeFields' @g inclDep
    Nothing -> typeFields' @g inclDep

instance GGraphQLFields f => GGraphQLFields (M1 D p f) where
  typeFields' = typeFields' @f

-- Enum
instance GGraphQLFields (M1 C c U1) where
  typeFields' = const Nothing

-- Enum
instance GGraphQLFields ((M1 C c U1) :+: g) where
  typeFields' = const Nothing

-- Union
instance GGraphQLFields ((M1 C c (M1 S s r)) :+: g) where
  typeFields' = const Nothing

data ConstructorProxy c (f :: * -> *) a = ConstructorProxy
type ConstructorProxy' c = ConstructorProxy c Proxy ()

-- Single field Object or Union
instance (Constructor c, GGraphQLFields f) => GGraphQLFields (M1 C c f) where
  typeFields' inclDep = if conIsRecord (ConstructorProxy :: ConstructorProxy' c) then
      typeFields' @f inclDep
    else
      Nothing

-----------------------------------

data Query a

instance GraphQLType Text where
  typeKind = ScalarKind
  typeName = "String"
  description = Just "String type"
  typeFields _ = Nothing
  enumValues _ = Nothing
  inputFields = Nothing

instance GraphQLScalar Text where

instance GraphQLType a => GraphQLType (Vector a) where
  typeKind = ListKind
  typeName = "List"
  typeFields _ = Nothing
  enumValues _ = Nothing
  inputFields = Nothing
  ofType = Just $ asGQLType @a

instance GraphQLType a => GraphQLType [a] where
  typeKind = ListKind
  typeName = "List"
  typeFields _ = Nothing
  enumValues _ = Nothing
  inputFields = Nothing
  ofType = Just $ asGQLType @a

-- instance GraphQLType a => GraphQLType (Maybe a) where
  -- name _ = 

-----------------------

data IFace = IFace {
  a :: Text
} deriving (Generic)

instance GraphQLType IFace where
  typeKind = InterfaceKind

data ExEnum = ExA | ExB
  deriving (Generic)

instance GraphQLType ExEnum

data ExUnion = ExInt Int | ExStr Text
  deriving (Generic)

instance GraphQLType ExUnion

data ExObj = ExObj {
  a :: Text,
  b :: BArgs -> Text,
  c :: ExEnum
} deriving (Generic)

instance GraphQLType ExObj

data ExObj2 = ExObj2 {
  ref :: ExObj
} deriving (Generic)

instance GraphQLType ExObj2

data BArgs = BArgs {
  i :: Int
} deriving (Generic)

type ExF f = HKD ExObj f

data ExG = ExG {
  a :: GQLField,
  b :: GQLField
}

data ExR m = ExR {
  a :: m Text,
  b :: BArgs -> m Text
}
