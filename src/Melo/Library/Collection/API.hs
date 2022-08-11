{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.API where

import Control.Lens hiding (from, lens, (|>))
import Data.Generics.Labels ()
import Data.Kind
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Data.UUID (fromText)
import qualified Data.Vector as V
import Data.Vector (Vector)
import GHC.Generics hiding (from)
import Melo.Common.FileSystem
import Melo.Common.Logging
import Melo.Database.Repo as Repo
import Melo.Format ()
import Melo.Format.Metadata ()
import Melo.GraphQL.Where
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import qualified Melo.Library.Collection.Types as Ty
import Melo.Library.Source.API as SrcApi
import qualified Melo.Library.Source.Repo as SrcRepo
import Network.URI
import Witch

resolveCollections ::
  ( CollectionRepository m,
    SrcRepo.SourceRepository m,
    FileSystem m
  ) =>
  CollectionsArgs ->
  ResolverQ e m (Vector (Collection (Resolver QUERY e m)))
resolveCollections (CollectionsArgs (Just CollectionWhere {..})) =
  case id of
    Just idExpr -> case idExpr of
      WhereEqExpr (EqExpr x) -> case fromText x of
        Just uuid -> lift $ fmap from <$> getByKey @Ty.Collection (V.singleton (Ty.CollectionRef uuid))
        Nothing -> fail $ "invalid collection id " <> show x
      WhereInExpr (InExpr x) -> case allJust (fmap fromText x) of
        Just uuids -> lift $ fmap from <$> getByKey @Ty.Collection (V.fromList $ Ty.CollectionRef <$> uuids)
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
      Nothing -> lift $ fmap (fmap from) $ getAll @Ty.Collection
  where
    allJust :: [Maybe a] -> Maybe [a]
    allJust [] = Just []
    allJust (Just a : as) = fmap (a :) (allJust as)
    allJust (Nothing : _) = Nothing
resolveCollections _ =
  lift $
    fmap (fmap from) $ getAll @Ty.Collection

data Collection m = Collection
  { id :: Ty.CollectionRef,
    rootUri :: Text,
    name :: Text,
    watch :: Bool,
    kind :: Text,
    sources :: CollectionSourcesArgs -> m (Vector (SrcApi.Source m)),
    sourceGroups :: m (Vector (SrcApi.SourceGroup m))
  }
  deriving (Generic)

instance Typeable m => GQLType (Collection m)

--  type KIND (Collection m) = INTERFACE

--instance Applicative m => From Ty.Collection (Collection m) where
--  from s =
--    Collection
--      { id = toText $ s ^. #ref . coerced,
--        format = "",
--        metadata = from $ s ^. #metadata,
--        sourceName = getCollectionName (T.pack $ show $ s ^. #source),
--        sourceUri = T.pack $ show $ s ^. #source,
--        downloadUri = "/source/" <> toText (s ^. #ref . coerced),
--        length = pure 100
--      }

instance
  ( SrcRepo.SourceRepository m,
    FileSystem m,
    WithOperation o
  ) =>
  From Ty.Collection (Collection (Resolver o e m))
  where
  from s =
    Collection
      { id = s ^. #id,
        name = s ^. #name,
        rootUri = s ^. #root_uri,
        watch = s ^. #watch,
        kind = s ^. #kind,
        sources = SrcApi.resolveCollectionSources (s ^. #id),
        sourceGroups = SrcApi.resolveCollectionSourceGroups (s ^. #id)
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

data CollectionMutation (m :: Type -> Type) = CollectionMutation
  { add :: AddCollectionArgs -> m (Collection m),
    delete :: DeleteCollectionArgs -> m (),
    deleteAll :: m ()
  }
  deriving (Generic)

instance Typeable m => GQLType (CollectionMutation m)

collectionMutation ::
  forall m e.
  ( CollectionRepository m,
    CollectionService m,
    SrcRepo.SourceRepository m,
    FileSystem m,
    Logging m
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
  ( CollectionService m,
    CollectionRepository m,
    SrcRepo.SourceRepository m,
    FileSystem m
  ) =>
  AddCollectionArgs ->
  ResolverM e m (Collection (Resolver MUTATION e m))
addCollectionImpl AddCollectionArgs {..} = do
  ref <- lift $ addCollection newCollection
  cs <- lift $ getByKey @Ty.Collection (V.singleton ref)
  let cs' = from <$> cs
  case firstOf traverse cs' of
    Just c -> pure c
    Nothing -> fail "failed to add collection"

data DeleteCollectionArgs = DeleteCollectionArgs
  { id :: Ty.CollectionRef
  }
  deriving (Generic)

instance GQLType DeleteCollectionArgs where
  type KIND DeleteCollectionArgs = INPUT

deleteCollectionImpl ::
  (CollectionRepository m, Logging m) =>
  DeleteCollectionArgs ->
  ResolverM e m ()
deleteCollectionImpl args = lift $ Repo.delete (V.singleton args.id)

deleteAllCollectionsImpl ::
  (CollectionRepository m) =>
  ResolverM e m ()
deleteAllCollectionsImpl = lift do
  collections <- getAll
  Repo.delete (fmap (^. #id) collections)
