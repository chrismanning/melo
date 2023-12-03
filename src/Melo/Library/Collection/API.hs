{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.API where

import Control.Concurrent.Classy
import Control.Foldl qualified as F
import Control.Foldl (PrimMonad)
import Data.Generics.Labels ()
import Data.Morpheus.Types
import Data.Text qualified as T
import Data.Typeable
import Data.UUID (fromText)
import Data.Vector qualified as V
import Melo.Common.Config
import Melo.Common.FileSystem
import Melo.Common.Tracing
import Melo.Common.Uuid
import Melo.Database.Repo as Repo
import Melo.Format ()
import Melo.Format.Metadata ()
import Melo.GraphQL.Where
import Melo.Library.Release.Repo
import Melo.Library.Release.ArtistName.Repo
import Melo.Library.Artist.Name.Repo
import Melo.Library.Collection.Aggregate
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Types qualified as Ty
import Melo.Library.Source.API as SrcApi
import Melo.Library.Source.Transform qualified as Tr
import Melo.Library.Track.ArtistName.Repo
import Melo.Library.Track.Repo
import Melo.Lookup.Covers
import Network.URI
import Streaming.Prelude qualified as S

resolveCollections ::
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    UuidGenerator m
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
    sources :: CollectionSourcesArgs -> m (Vector (SrcApi.Source m))
  }
  deriving (Generic)

instance Typeable m => GQLType (Collection m)

instance
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    UuidGenerator m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
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
        sources = SrcApi.resolveCollectionSources e.id
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
  deriving (Generic, GQLType)

data CollectionWhere = CollectionWhere
  { id :: Maybe Where,
    rootUri :: Maybe Where
  }
  deriving (Generic, GQLType)

data CollectionMutation (m :: Type -> Type) = CollectionMutation
  { add :: AddCollectionArgs -> m (Collection m),
    delete :: DeleteCollectionArgs -> m (Maybe Ty.CollectionRef),
    deleteAll :: m (Vector Ty.CollectionRef)
  }
  deriving (Generic)

instance Typeable m => GQLType (CollectionMutation m)

collectionMutation ::
  forall m e.
  ( Tr.MonadSourceTransform m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    MonadConc m,
    CollectionAggregate m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    UuidGenerator m
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
  deriving (Generic, GQLType)

addCollectionImpl ::
  forall m e.
  ( Tr.MonadSourceTransform m,
    MonadConc m,
    CollectionAggregate m,
    ReleaseRepository m,
    ReleaseArtistNameRepository m,
    ArtistNameRepository m,
    TrackArtistNameRepository m,
    TrackRepository m,
    FileSystem m,
    CoverService m,
    ConfigService m,
    Tracing m,
    Typeable m,
    UuidGenerator m
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
  deriving (Generic, GQLType)

deleteCollectionImpl ::
  CollectionAggregate m =>
  DeleteCollectionArgs ->
  ResolverM e m (Maybe Ty.CollectionRef)
deleteCollectionImpl args = lift $ deleteCollection args.id

deleteAllCollectionsImpl ::
  (CollectionRepository m, CollectionAggregate m, PrimMonad m) =>
  ResolverM e m (Vector Ty.CollectionRef)
deleteAllCollectionsImpl = lift do
  collections <- getAll @Ty.CollectionEntity
  S.each collections
    & S.map (.id)
    & S.mapMaybeM deleteCollection
    & F.impurely S.foldM_ F.vectorM
