{-# LANGUAGE UndecidableInstances #-}

module Melo.Library.API where

import Control.Concurrent.Classy
import Control.Monad.Reader
import Data.Kind
import Data.Morpheus.Types
import Data.Typeable
import Data.Vector (Vector)
import GHC.Generics hiding (from)
import GHC.OverloadedLabels ()
import Melo.Common.FileSystem
import Melo.Library.Collection.API
import Melo.Library.Collection.Aggregate
import Melo.Library.Source.API
import Melo.Library.Source.Transform qualified as Tr

data LibraryQuery m = LibraryQuery
  { collections :: CollectionsArgs -> m (Vector (Collection m))
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryQuery m)

resolveLibrary ::
  ( Tr.MonadSourceTransform m,
    MonadConc m
  ) =>
  ResolverQ e m LibraryQuery
resolveLibrary =
  lift $
    pure
      LibraryQuery
        { collections = resolveCollections
        }

data LibraryMutation (m :: Type -> Type) = LibraryMutation
  { transformSources :: TransformSources m,
    collection :: m (CollectionMutation m)
  }
  deriving (Generic)

instance Typeable m => GQLType (LibraryMutation m)

libraryMutation ::
  ( MonadIO m,
    CollectionAggregate m,
    Tr.MonadSourceTransform m,
    MonadConc m,
    FileSystem m
  ) =>
  ResolverM e (m :: Type -> Type) LibraryMutation
libraryMutation =
  lift $
    pure
      LibraryMutation
        { transformSources = transformSourcesImpl,
          collection = collectionMutation
        }
