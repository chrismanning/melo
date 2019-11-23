{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Melo.GraphQL.Resolve where

import Control.Applicative
import Control.Lens (Contravariant, Optic', Profunctor, (^.), (.~), (&))
import qualified Control.Lens as L
import Control.Monad.Catch
import qualified Data.Aeson as A
import Data.Aeson as A (FromJSON(..), ToJSON(..), (.=))
import Data.Barbie
import Data.Generic.HKD
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Singletons
import Data.Tagged
import Data.Text as T
import Data.Traversable
import Data.Vector as V
import GHC.Generics as G
import GHC.TypeLits as TL
import Language.GraphQL.AST.Core as QL hiding (Query)

import Melo.GraphQL.Introspect as I
import Melo.API.Haxl

data GraphQLException = FieldNotFound {fieldName :: Text, typeName :: Text}
  deriving (Show, Exception)

data Location
  = Location
      { line :: Int,
        column :: Int
      }
  deriving (Eq, Show, Generic, ToJSON)

data Error
  = Error
      { message :: Text,
        locations :: [Location],
        path :: [Text],
        extensions :: Maybe A.Value
      }
  deriving (Eq, Show, Generic, ToJSON)

data Response
  = Response
      { _data :: A.Value,
        errors :: [Error]
      }

instance ToJSON Response where
  toJSON rs = A.object ["data" .= _data rs, "errors" .= errors rs]

fieldName' :: QL.Field -> Text
fieldName' (Field _ name _ _) = name

fieldArgs' :: QL.Field -> [QL.Argument]
fieldArgs' (Field _ _ as _) = as

fieldSelections' :: QL.Field -> [QL.Selection]
fieldSelections' (Field _ _ _ fs) = fs

collectFields :: [QL.Field] -> [(Text, QL.Field)]
collectFields = foldMap (\field' -> [(fieldName' field', field')])

responseKey_ :: QL.Field -> Text
responseKey_ (QL.Field alias name _ _) = fromMaybe name alias

responseKey :: (Profunctor p, Contravariant f) => Optic' p f QL.Field Text
responseKey = L.to responseKey_

getFieldNamed :: Text -> [QL.Field] -> Maybe QL.Field
getFieldNamed _ [] = Nothing
getFieldNamed fn (f : fs) =
  let (Field _ fn' _ _) = f
    in if fn' == fn then Just f else getFieldNamed fn fs

-----------------------

class ObjectResolver (m :: * -> *) a where
  type ResolverContext a = r | r -> a

  resolveFieldValue :: ResolverContext a -> QL.Field -> m A.Value
  default resolveFieldValue :: (
    Generic (GResolver m a),
    GenericResolver m a,
    GObjectResolver m (Rep (GResolver m a)) (ResolverContext a),
    GraphQLType a,
    MonadThrow m
    ) =>
    ResolverContext a -> QL.Field -> m A.Value
  resolveFieldValue c f = case resolveFieldValue' (from $ genericResolver @m @a) c f of
    Just res -> res
    Nothing -> throwM FieldNotFound
      { fieldName = fieldName' f,
        typeName = fromMaybe "" $ I.typeName @a
      }

resolveFieldValues :: forall m a. (Monad m, ObjectResolver m a) => ResolverContext a -> [QL.Field] -> m A.Value
resolveFieldValues ctx fields = do
  resultMap <- for fields $ \field' -> (field' ^. responseKey,) <$> resolveFieldValue @m @a ctx field'
  pure $ toJSON $ Map.fromList resultMap

class GenericResolver m a where
  type GResolver m a
  type GResolver m a = ResolverM m a
  genericResolver :: (GraphQLType a) => GResolver m a

class GObjectResolver (m :: * -> *) (f :: * -> *) ctx where
  resolveFieldValue' :: f a -> ctx -> QL.Field -> Maybe (m A.Value)

instance (GObjectResolver m f ctx) => GObjectResolver m (M1 D d f) ctx where
  resolveFieldValue' (M1 a) = resolveFieldValue' a

instance (GObjectResolver m f ctx) => GObjectResolver m (M1 C c f) ctx where
  resolveFieldValue' (M1 a) = resolveFieldValue' a

instance (CoerceArgs (FieldArguments a), SingI n) => GObjectResolver m (M1 S ('MetaSel ('Just n) x y z) (Rec0 (Resolve m ctx a))) ctx where
  resolveFieldValue' (M1 (K1 (Resolve r))) ctx (Field _ name args selections)
    | name == fromSing (sing @n) = Just $ r ctx (coerceArgumentValues @(FieldArguments a) args) [s | SelectionField s <- selections]
    | otherwise = Nothing

instance (GObjectResolver m f ctx, GObjectResolver m g ctx) => GObjectResolver m (f :*: g) ctx where
  resolveFieldValue' (f :*: g) c f' = resolveFieldValue' f c f' <|> resolveFieldValue' g c f'

class CoerceArgs a where
  coerceArgumentValues :: [Argument] -> a

instance CoerceArgs () where
  coerceArgumentValues = const ()

type family FieldArguments a where
  FieldArguments (Tagged _ a) = FieldArguments a
  FieldArguments (a -> b -> c) = TypeError
    ('TL.Text "GraphQL methods may only have a single argument"
      ':<>: 'TL.Text "This argument must be a record defining named query arguments")
  FieldArguments (a -> b) = FieldArgument a
  FieldArguments a = ()

newtype FieldArgument a = FieldArgument a

instance FromJSON a => CoerceArgs (FieldArgument a) where
  coerceArgumentValues args = case A.fromJSON $ A.object (fmap convertArg args) of
    A.Error e -> error $ "Error coercing argument values: " <> e
    A.Success a -> FieldArgument a
    where
    convertArg (Argument name val) = name .= convertVal val
    convertVal val = val'
      where
        val' = case val of
          ValueInt i -> toJSON i
          ValueFloat f -> toJSON f
          ValueString s -> toJSON s
          ValueBoolean b -> toJSON b
          ValueNull -> A.Null
          ValueEnum n -> A.String n
          ValueList l -> A.Array $ V.fromList $ fmap convertVal l
          ValueObject o -> A.object (fmap (\(ObjectField name v) -> name .= convertVal v) o)

-----------------------

type family ResolveResultT a where
  ResolveResultT (Tagged _ a) = ResolveResultT a
  ResolveResultT (a -> b -> c) = TypeError
    ('TL.Text "GraphQL methods may only have a single argument"
      ':<>: 'TL.Text "This argument must be a record defining named query arguments")
  ResolveResultT (a -> b) = ResolveResultT b
  ResolveResultT a = a

pureResolver :: (ToJSON (ResolveResultT a), Monad m) =>
    ResolveResultT a -> Resolve m c a
pureResolver a = Resolve (\_ _ _ -> pure $ toJSON a)

pureCtxResolver :: (ToJSON (ResolveResultT a), Monad m) => (c -> ResolveResultT a) -> Resolve m c a
pureCtxResolver a = Resolve (\c _ _ -> pure $ toJSON (a c))

nullresolver :: (Applicative m) => Resolve m c a
nullresolver = Resolve (\_ _ _ -> pure A.Null)

-- introspectionResolver :: GQLSchema -> Resolve m c a

data Resolve m c a where
  Resolve :: (c -> FieldArguments a -> [QL.Field] -> m A.Value) -> Resolve m c a

type family UnwrapNull a where
  UnwrapNull (NonNull a) = a
  UnwrapNull (Maybe a) = a
  UnwrapNull a = a

type ExObjResolver = HaxlResolver ExObj

type ResolverM m a = HKD a (Resolve m (ResolverContext a))

type HaxlResolver a = ResolverM Haxl a

-----------------------

data IFace = IFace {
  a :: Text
} deriving (Generic)

instance GraphQLType IFace where
  type TypeKind IFace = 'InterfaceKind

data ExEnum = ExA | ExB
  deriving (Generic, ToJSON)

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
  d :: Deprecated Int,
  e :: Maybe Int
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
} deriving (Generic, FromJSON)

instance GraphQLType BArgs where
  type TypeKind BArgs = 'InputObjectKind

type ExObjF f = HKD ExObj f

instance ObjectResolver Maybe ExObj where
  type ResolverContext ExObj = ()

instance GenericResolver Maybe ExObj where
  genericResolver =
    buniq nullresolver
      & field @"a" .~ (pureResolver "a value")
      & field @"b" .~ (pureResolver "b value")
      & field @"c" .~ (pureResolver ExA)
      & field @"d" .~ (pureResolver 321)
      & field @"e" .~ (pureResolver (Just 321))
