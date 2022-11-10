{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.API where

import Control.Concurrent.Classy
import Data.Generics.Labels ()
import Data.Kind
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable
import Data.UUID (fromText)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Melo.Common.FileSystem
import Melo.Database.Repo as Repo
import Melo.Format ()
import Melo.Format.Metadata ()
import Melo.GraphQL.Where
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Source.API as SrcApi
import Melo.Library.Source.Transform qualified as Tr
import Network.URI
import Witch

resolveCollections ::
  ( Tr.MonadSourceTransform m,
    MonadConc m
  ) =>
  CollectionsArgs ->
  ResolverQ e m (Vector (Collection (Resolver QUERY e m)))
resolveCollections (CollectionsArgs (Just CollectionWhere {..})) =
  case id of
    Just idExpr -> case idExpr of
      WhereEqExpr (EqExpr x) -> case fromText x of
        Just uuid -> lift $ fmap from <$> getByKey @Ty.CollectionEntity (V.singleton (Ty.CollectionRef uuid))
        Nothing -> fail $ "invalid collection id " <> show x
      WhereInExpr (InExpr x) -> case allJust (fmap fromText x) of
        Just uuids -> lift $ fmap from <$> getByKey @Ty.CollectionEntity (V.fromList $ Ty.CollectionRef <$> uuids)
        Nothing -> fail $ "invalid collection id in " <> show x
      _unknownWhere -> fail "invalid where clause for Collection.id"
    Nothing -> case rootUri of
      Just rootUriExpr -> case rootUriExpr of
        WhereEqExpr (EqExpr x) -> case parseURI (T.unpack x) of
          Just uri -> lift $ fmap from <$> getByUri (V.singleton uri)
          Nothing -> fail $ "invalid collection uri " <> show x
        WhereInExpr (InExpr x) -> case allJust (fmap (parseURI . T.unpack) x) of
          Just uris -> lift $ fmap from <$> getByUri (V.fromList uris)
          Nothing -> fail $ "invalid collection id in " <> show x
        _unknownWhere -> fail "invalid where clause for Collection.rootUri"
      Nothing -> lift $ fmap (fmap from) $ getAll @Ty.CollectionEntity
  where
    allJust :: [Maybe a] -> Maybe [a]
    allJust [] = Just []
    allJust (Just a : as) = fmap (a :) (allJust as)
    allJust (Nothing : _) = Nothing
resolveCollections _ =
  lift $
    fmap (fmap from) $
      getAll @Ty.CollectionEntity

data Collection m = Collection
  { id :: Ty.CollectionRef,
    rootUri :: Text,
    name :: Text,
    watch :: Bool,
    kind :: Text,
    sources :: CollectionSourcesArgs -> m (Vector (SrcApi.Source m)),
    sourceGroups :: CollectionSourceGroupsArgs -> m (Vector (SrcApi.SourceGroup m))
  }
  deriving (Generic)

instance Typeable m => GQLType (Collection m)

instance
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    WithOperation o
  ) =>
  From Ty.CollectionEntity (Collection (Resolver o e m))
  where
  from e =
    Collection
      { id = e.id,
        name = e.name,
        rootUri = e.root_uri,
        watch = e.watch,
        kind = e.kind,
        sources = SrcApi.resolveCollectionSources e.id,
        sourceGroups = \args -> SrcApi.resolveCollectionSourceGroups e.id args.groupByMappings
      }

data LocalFileCollection m = LocalFileCollection
  { id :: Ty.CollectionRef,
    rootUri :: Text,
    name :: Text,
    watch :: Bool,
    kind :: Text,
    sources :: SourcesArgs -> m [SrcApi.Source m],
    rootPath :: m Text
  }
  deriving (Generic)

data CollectionsArgs = CollectionsArgs
  { where' :: Maybe CollectionWhere
  }
  deriving (Generic)

instance GQLType CollectionsArgs where
  type KIND CollectionsArgs = INPUT

data CollectionWhere = CollectionWhere
  { id :: Maybe Where,
    rootUri :: Maybe Where
  }
  deriving (Generic)

instance GQLType CollectionWhere where
  type KIND CollectionWhere = INPUT

data Unit = Unit
  deriving (Generic)

instance GQLType Unit

data CollectionMutation (m :: Type -> Type) = CollectionMutation
  { add :: AddCollectionArgs -> m (Collection m),
    delete :: DeleteCollectionArgs -> m Unit,
    deleteAll :: m Unit
  }
  deriving (Generic)

instance Typeable m => GQLType (CollectionMutation m)

collectionMutation ::
  forall m e.
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    CollectionAggregate m
  ) =>
  ResolverM e (m :: Type -> Type) CollectionMutation
collectionMutation =
  lift $
    pure
      CollectionMutation
        { add = addCollectionImpl @m,
          delete = deleteCollectionImpl @m,
          deleteAll = deleteAllCollectionsImpl @m
        }

data AddCollectionArgs = AddCollectionArgs
  { newCollection :: Ty.NewCollection
  }
  deriving (Generic)

instance GQLType AddCollectionArgs where
  type KIND AddCollectionArgs = INPUT

addCollectionImpl ::
  forall m e.
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    CollectionAggregate m,
    FileSystem m
  ) =>
  AddCollectionArgs ->
  ResolverM e m (Collection (Resolver MUTATION e m))
addCollectionImpl AddCollectionArgs {..} = do
  ref <- lift $ addCollection newCollection
  lift (fmap from <$> getSingle @Ty.CollectionEntity ref) >>= \case
    Just c -> pure c
    Nothing -> fail "failed to add collection"

data DeleteCollectionArgs = DeleteCollectionArgs
  { id :: Ty.CollectionRef
  }
  deriving (Generic)

instance GQLType DeleteCollectionArgs where
  type KIND DeleteCollectionArgs = INPUT

deleteCollectionImpl ::
  CollectionAggregate m =>
  DeleteCollectionArgs ->
  ResolverM e m Unit
deleteCollectionImpl args = lift $ deleteCollection args.id >> pure Unit

deleteAllCollectionsImpl ::
  (CollectionRepository m, CollectionAggregate m) =>
  ResolverM e m Unit
deleteAllCollectionsImpl = lift do
  collections <- getAll
  mapM_ (deleteCollection . (.id)) collections
  pure Unit
