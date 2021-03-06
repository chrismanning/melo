{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.Collection.API where

import Basement.From
import Control.Lens hiding (from, lens, (|>))
import Data.Generics.Labels ()
import Data.Kind
import Data.Morpheus.Kind
import Data.Morpheus.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (fromText, toText)
import Database.Beam as B hiding (char, insert)
import Melo.Common.FileSystem
import Melo.Common.Logging
import qualified Melo.Database.Model as DB
import Melo.Format ()
import Melo.Format.Metadata ()
import Melo.GraphQL.Where
import Melo.Library.Collection.Repo
import Melo.Library.Collection.Service
import Melo.Library.Collection.Types
import Melo.Library.Source.API as SrcApi
import qualified Melo.Library.Source.Repo as SrcRepo
import Network.URI

resolveCollections ::
  ( CollectionRepository m,
    SrcRepo.SourceRepository m,
    FileSystem m
  ) =>
  CollectionsArgs ->
  ResolverQ e m [Collection (Resolver QUERY e m)]
resolveCollections (CollectionsArgs (Just CollectionWhere {..})) =
  case id of
    Just idExpr -> case idExpr of
      WhereEqExpr (EqExpr x) -> case fromText x of
        Just uuid -> lift $ fmap from <$> getCollections [DB.CollectionKey uuid]
        Nothing -> fail $ "invalid collection id " <> show x
      WhereInExpr (InExpr x) -> case allJust (fmap fromText x) of
        Just uuids -> lift $ fmap from <$> getCollections (DB.CollectionKey <$> uuids)
        Nothing -> fail $ "invalid collection id in " <> show x
      _unknownWhere -> fail "invalid where clause for Collection.id"
    Nothing -> case rootUri of
      Just rootUriExpr -> case rootUriExpr of
        WhereEqExpr (EqExpr x) -> case parseURI (T.unpack x) of
          Just uri -> lift $ fmap from <$> getCollectionsByUri [uri]
          Nothing -> fail $ "invalid collection uri " <> show x
        WhereInExpr (InExpr x) -> case allJust (fmap (parseURI . T.unpack) x) of
          Just uris -> lift $ fmap from <$> getCollectionsByUri uris
          Nothing -> fail $ "invalid collection id in " <> show x
        _unknownWhere -> fail "invalid where clause for Collection.rootUri"
      Nothing -> lift $ fmap (fmap from) getAllCollections
  where
    allJust :: [Maybe a] -> Maybe [a]
    allJust [] = Just []
    allJust (Just a : as) = fmap (a :) (allJust as)
    allJust (Nothing : _) = Nothing
resolveCollections _ =
  lift $
    fmap (fmap from) getAllCollections

data Collection m = Collection
  { id :: Text,
    rootUri :: Text,
    name :: Text,
    watch :: Bool,
    kind :: Text,
    sources :: SourcesArgs -> m [SrcApi.Source m],
    sourceGroups :: m [SrcApi.SourceGroup m]
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
  From DB.Collection (Collection (Resolver o e m))
  where
  from s =
    Collection
      { id = toText (s ^. #id),
        name = s ^. #name,
        rootUri = s ^. #root_uri,
        watch = s ^. #watch,
        kind = s ^. #kind,
        sources = SrcApi.resolveSources,
        sourceGroups = SrcApi.resolveSourceGroups
      }

data LocalFileCollection m = LocalFileCollection
  { id :: Text,
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
  { newCollection :: NewCollection
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
  CollectionRef ref <- lift $ addCollection newCollection
  cs <- lift $ getCollections [DB.CollectionKey ref]
  let cs' = from <$> cs
  case cs' of
    (c : _) -> pure c
    [] -> fail "failed to add collection"

data DeleteCollectionArgs = DeleteCollectionArgs
  { id :: Text
  }
  deriving (Generic)

instance GQLType DeleteCollectionArgs where
  type KIND DeleteCollectionArgs = INPUT

deleteCollectionImpl ::
  (CollectionRepository m, Logging m) =>
  DeleteCollectionArgs ->
  ResolverM e m ()
deleteCollectionImpl DeleteCollectionArgs {..} =
  case fromText id of
    Just uuid -> lift $ deleteCollections [DB.CollectionKey uuid]
    Nothing -> lift $ $(logWarn) $ "invalid UUID " <> id

deleteAllCollectionsImpl ::
  (CollectionRepository m) =>
  ResolverM e m ()
deleteAllCollectionsImpl = lift deleteAllCollections
