module Melo.GraphQL.Resolve where

import Control.Applicative
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Data.Aeson as A
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text as T
import Data.Traversable
import GHC.Generics as G hiding (to)
import GHC.OverloadedLabels ()
import Language.GraphQL.AST.Core hiding (Query)

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

class GraphQlType a where

  typeName :: a -> Text

  resolver :: a

  default typeName :: (Generic a, GTypeName (Rep a)) => a -> Text
  typeName _ = typeName' (G.from (undefined :: a))

class GTypeName f where
  typeName' :: f a -> Text

instance (Datatype d) => GTypeName (D1 d f) where
  typeName' = T.pack . datatypeName

class (Monad m, MonadThrow m, GraphQlType a) => QLFieldResolve m a where

  data QLFieldContext a

  resolveField :: a -> QLFieldContext a -> Field -> m A.Value

  resolveFields :: a -> QLFieldContext a -> [Field] -> m A.Value

  default resolveFields :: a -> QLFieldContext a -> [Field] -> m A.Value
  resolveFields a ctx fields = do
    resultMap <- for fields $ \field -> (field ^. responseKey,) <$> resolveField a ctx field
    pure $ toJSON $ Map.fromList resultMap

  default resolveField :: (Generic a, GFieldResolve m (QLFieldContext a) (Rep a)) => a -> QLFieldContext a -> Field -> m A.Value
  resolveField a ctx field = case resolveField' (G.from a) (fieldName' field) ctx (fieldArgs' field) (fieldFields' field) of
    Just res -> res
    Nothing -> throwM FieldNotFound
      { fieldName = fieldName' field,
        typeName = typeName a
      }

class (Monad m) => GFieldResolve m ctx (f :: * -> *) where
  resolveField' :: f p -> Text -> ctx -> [Argument] -> [Field] -> Maybe (m A.Value)

instance (Monad m) => GFieldResolve m ctx U1 where
  resolveField' _ _ _ _ _ = Nothing

instance (Monad m, Selector s) => GFieldResolve m ctx (M1 S s (Rec0 (FieldResolver m ctx a))) where
  resolveField' m@(M1 (K1 src)) n ctx args fs
    | T.pack (selName m) == n = Just $ src ctx args fs
    | otherwise = Nothing

instance (Monad m, GFieldResolve m ctx f, GFieldResolve m ctx g) => GFieldResolve m ctx (f :*: g) where
  resolveField' (a :*: b) n ctx args fs = resolveField' a n ctx args fs <|> resolveField' b n ctx args fs

instance (Monad m, GFieldResolve m ctx f) => GFieldResolve m ctx (M1 D p f) where
  resolveField' (M1 src) = resolveField' src

instance (Monad m, GFieldResolve m ctx f) => GFieldResolve m ctx (M1 C p f) where
  resolveField' (M1 src) = resolveField' src

type FieldResolver m ctx a = ctx -> [Argument] -> [Field] -> m A.Value

fieldName' :: Field -> Text
fieldName' (Field _ name _ _) = name

fieldArgs' :: Field -> [Argument]
fieldArgs' (Field _ _ as _) = as

fieldFields' :: Field -> [Field]
fieldFields' (Field _ _ _ fs) = fs

collectFields :: [Field] -> [(Text, Field)]
collectFields = foldMap (\field -> [(fieldName' field, field)])

responseKey_ :: Field -> Text
responseKey_ (Field alias name _ _) = fromMaybe name alias

responseKey :: (Profunctor p, Contravariant f) => Optic' p f Field Text
responseKey = to responseKey_

getFieldNamed :: Text -> [Field] -> Maybe Field
getFieldNamed _ [] = Nothing
getFieldNamed fn (f : fs) =
  let (Field _ fn' _ _) = f
   in if fn' == fn then Just f else getFieldNamed fn fs
