{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.GraphQL.Introspect where

import Control.Lens ((^.))
import Control.Monad.Extra (whenJust)
import Control.Monad.ST
import Data.Generics.Labels ()
import qualified Data.HashTable.ST.Basic as H
import Data.Kind
import Data.List.NonEmpty as NE hiding (filter)
import Data.Proxy
import Data.Singletons
import Data.Singletons.TH
import Data.Tagged
import Data.Text as T hiding (filter)
import Data.Vector (Vector)
import GHC.Generics
import GHC.OverloadedLabels ()
import GHC.TypeLits as TL
import Network.URI (URI)

data GQLTypeKind
  = ScalarKind
  | ObjectKind
  | InterfaceKind
  | UnionKind
  | EnumKind
  | InputObjectKind
  | ListKind
  | NonNullKind
  deriving (Show, Eq, Generic, Enum)

$(genSingletons [''GQLTypeKind])

instance GraphQLType GQLTypeKind where
  type TypeKind GQLTypeKind = 'EnumKind

newtype NonNull a = NonNull a

data GQLSchema = GQLSchema
  { types :: NonEmpty GQLType,
    queryType :: GQLType,
    mutationType :: Maybe GQLType,
    subscriptionType :: Maybe GQLType
  }
  deriving (Generic)

instance GraphQLType GQLSchema where
  type TypeKind GQLSchema = 'ObjectKind

data FieldsArgs = FieldsArgs
  { includeDeprecated :: Bool
  }
  deriving (Generic)

instance GraphQLType FieldsArgs where
  type TypeKind FieldsArgs = 'InputObjectKind

data EnumValuesArgs = EnumValuesArgs
  { includeDeprecated :: Bool
  }
  deriving (Generic)

instance GraphQLType EnumValuesArgs where
  type TypeKind EnumValuesArgs = 'InputObjectKind

data GQLType = GQLType
  { kind :: GQLTypeKind,
    name :: Maybe Text,
    description :: Maybe Text,
    fields :: FieldsArgs -> Maybe [GQLField],
    interfaces :: Maybe [GQLType],
    possibleTypes :: Maybe [GQLType],
    enumValues :: EnumValuesArgs -> Maybe [GQLEnumValue],
    inputFields :: Maybe [GQLInputValue],
    ofType :: Maybe GQLType
  }
  deriving (Generic)

instance GraphQLType GQLType where
  type TypeKind GQLType = 'ObjectKind

data GQLField = GQLField
  { name :: Text,
    description :: Maybe Text,
    args :: [GQLInputValue],
    fieldType :: GQLType,
    isDeprecated :: Bool,
    deprecationReason :: Maybe Text
  }
  deriving (Generic)

instance GraphQLType GQLField where
  type TypeKind GQLField = 'ObjectKind

data GQLInputValue = GQLInputValue
  { name :: Text,
    description :: Maybe Text,
    inputValueType :: GQLType,
    defaultValue :: Maybe Text
  }
  deriving (Generic)

instance GraphQLType GQLInputValue where
  type TypeKind GQLInputValue = 'ObjectKind

data GQLEnumValue = GQLEnumValue
  { name :: Text,
    description :: Maybe Text,
    isDeprecated :: Bool,
    deprecationReason :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance GraphQLType GQLEnumValue where
  type TypeKind GQLEnumValue = 'ObjectKind

class GraphQLType a where
  type TypeKind a :: GQLTypeKind

  gqlType :: GQLType
  default gqlType :: GQLType
  gqlType =
    GQLType
      { kind = typeKind @a,
        name = typeName @a,
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

  typeName :: Maybe Text
  default typeName :: GTypeName (Rep a) => Maybe Text
  typeName = Just $ typeName' @(Rep a)

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

data DatatypeProxy d (f :: Type -> Type) a = DatatypeProxy

type DatatypeProxy' d = DatatypeProxy d Proxy ()

class GTypeName f where
  typeName' :: Text

instance (Datatype d) => GTypeName (D1 d f) where
  typeName' = T.pack $ datatypeName (DatatypeProxy :: DatatypeProxy' d)

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

type family FieldResultT a where
  FieldResultT (Tagged _ a) = FieldResultT a
  FieldResultT (a -> b -> c) =
    TypeError
      ( 'TL.Text "GraphQL methods may only have a single argument"
          ':<>: 'TL.Text "This argument must be a record defining named query arguments"
      )
  FieldResultT (a -> b) = FieldResultT b
  FieldResultT (Maybe a) = a
  FieldResultT a = NonNull a

data DeprecatedTag

type Deprecated a = Tagged DeprecatedTag a

type family IsFieldDeprecated a :: Bool where
  IsFieldDeprecated (Tagged DeprecatedTag a) = 'True
  IsFieldDeprecated (Tagged x a) = IsFieldDeprecated a
  IsFieldDeprecated a = 'False

data SelectorProxy s (f :: Type -> Type) a = SelectorProxy

type SelectorProxy' s = SelectorProxy s Proxy ()

class GGraphQLFields' f where
  typeObjectFields' :: Bool -> [GQLField]

instance GGraphQLFields' f => GGraphQLFields' (M1 D p (M1 C c f)) where
  typeObjectFields' = typeObjectFields' @f

instance GGraphQLFields' f => GGraphQLFields' (M1 C c f) where
  typeObjectFields' = typeObjectFields' @f

instance
  (Selector s, GraphQLType (FieldResultT a), SingI (IsFieldDeprecated a)) =>
  GGraphQLFields' (M1 S s (Rec0 a))
  where
  typeObjectFields' inclDep =
    let f =
          GQLField
            { name = T.pack $ selName (SelectorProxy :: SelectorProxy' s),
              fieldType = gqlType @(FieldResultT a),
              description = Nothing,
              args = [],
              isDeprecated = fromSing $ sing @(IsFieldDeprecated a),
              deprecationReason = Nothing
            }
     in if not inclDep && fromSing (sing @(IsFieldDeprecated a)) then [] else [f]

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

instance (Selector s, GraphQLType (FieldResultT a)) => GGraphQLInputFields' (M1 S s (Rec0 a)) where
  inputFields' =
    let f =
          GQLInputValue
            { name = T.pack $ selName (SelectorProxy :: SelectorProxy' s),
              description = Nothing,
              inputValueType = gqlType @(FieldResultT a),
              defaultValue = Nothing
            }
     in [f]

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
  enumValues' inclDep =
    let v =
          GQLEnumValue
            { name = T.pack $ conName (ConstructorProxy :: ConstructorProxy' c),
              description = Nothing,
              isDeprecated = False,
              deprecationReason = Nothing
            }
     in filter (\x -> inclDep || not (x ^. #isDeprecated)) [v]

instance (GGraphQLEnumValue' f, GGraphQLEnumValue' g) => GGraphQLEnumValue' (f :+: g) where
  enumValues' inclDep = enumValues' @f inclDep <> enumValues' @g inclDep

data ConstructorProxy c (f :: Type -> Type) a = ConstructorProxy

type ConstructorProxy' c = ConstructorProxy c Proxy ()

-----------------------------------

instance GraphQLType Bool where
  type TypeKind Bool = 'ScalarKind
  typeKind = ScalarKind
  typeName = Just "Bool"
  description = Just "Bool type"

instance GraphQLType Text where
  type TypeKind Text = 'ScalarKind
  typeKind = ScalarKind
  typeName = Just "String"
  description = Just "String type"

instance GraphQLType Int where
  type TypeKind Int = 'ScalarKind
  typeName = Just "Int"
  description = Just "Int type"

instance GraphQLType Integer where
  type TypeKind Integer = 'ScalarKind
  typeName = Just "Integer"
  description = Just "Arbitrary precision Int type"

instance GraphQLType a => GraphQLType (Vector a) where
  type TypeKind (Vector a) = 'ListKind
  typeName = Nothing
  description = fmap ("List of " <>) (description @a)
  ofType = Just $ gqlType @a

instance GraphQLType a => GraphQLType [a] where
  type TypeKind [a] = 'ListKind
  typeName = Nothing
  description = fmap ("List of " <>) (description @a)
  ofType = Just $ gqlType @a

instance GraphQLType a => GraphQLType (NonEmpty a) where
  type TypeKind (NonEmpty a) = 'ListKind
  typeName = Nothing
  description = fmap ("List of " <>) (description @a)
  ofType = Just $ gqlType @a

instance GraphQLType a => GraphQLType (NonNull a) where
  type TypeKind (NonNull a) = 'NonNullKind
  typeName = Nothing
  ofType = Just $ gqlType @a

instance GraphQLType a => GraphQLType (Maybe a) where
  type TypeKind (Maybe a) = TypeKind a
  typeKind = typeKind @a
  typeName = typeName @a
  description = description @a
  typeFields = typeFields @a
  interfaces = interfaces @a
  possibleTypes = possibleTypes @a
  typeEnumValues = typeEnumValues @a
  typeInputFields = typeInputFields @a
  ofType = ofType @a

instance GraphQLType URI where
  type TypeKind URI = 'ScalarKind
  typeKind = ScalarKind
  typeName = Just "String"
  description = Just "String type"

----------------------------

class SchemaIntrospection s where
  introspectSchema :: s -> GQLSchema

data Schema q m s = Schema
  { query :: q,
    mutation :: m,
    schema :: s
  }

instance (GraphQLType q) => SchemaIntrospection (Schema q m s) where
  introspectSchema _ =
    let types = collectTypes (gqlType @q)
     in GQLSchema
          { types = types,
            queryType = gqlType @q,
            mutationType = Nothing,
            subscriptionType = Nothing
          }

type GQLTypeTable s = H.HashTable s Text GQLType

collectTypes :: GQLType -> NonEmpty GQLType
collectTypes t = runST s
  where
    s :: ST s (NonEmpty GQLType)
    s = do
      ts <- H.new
      collectTypes' ts t
      types <- H.foldM (\a (_, v) -> pure (v : a)) [] ts
      pure $ NE.fromList types
    collectTypes' :: GQLTypeTable s -> GQLType -> ST s ()
    collectTypes' ts t' = case t' ^. #name of
      Nothing -> wrapperOfType ts t'
      Just name' ->
        H.lookup ts name' >>= \case
          Nothing -> do
            H.insert ts name' t'
            fields' ts t'
            interfaces' ts t'
            possibleTypes' ts t'
            inputTypes' ts t'
          Just _ -> wrapperOfType ts t'
    fields' :: GQLTypeTable s -> GQLType -> ST s ()
    fields' ts GQLType {fields} =
      case fields (FieldsArgs True) of
        Nothing -> pure ()
        Just fs -> mapM_ (\f -> collectTypes' ts (f ^. #fieldType)) fs
    interfaces' :: GQLTypeTable s -> GQLType -> ST s ()
    interfaces' ts t' = whenJust (t' ^. #interfaces) $ mapM_ (collectTypes' ts)
    possibleTypes' :: GQLTypeTable s -> GQLType -> ST s ()
    possibleTypes' ts t' = whenJust (t' ^. #possibleTypes) $ mapM_ (collectTypes' ts)
    wrapperOfType :: GQLTypeTable s -> GQLType -> ST s ()
    wrapperOfType ts t' = whenJust (t' ^. #ofType) $ collectTypes' ts
    inputTypes' :: GQLTypeTable s -> GQLType -> ST s ()
    inputTypes' ts t' =
      case t' ^. #inputFields of
        Nothing -> pure ()
        Just fs -> mapM_ (\f -> collectTypes' ts (f ^. #inputValueType)) fs
