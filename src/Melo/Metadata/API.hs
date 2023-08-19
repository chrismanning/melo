{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Metadata.API where

import Control.Concurrent.Classy
import Control.Monad.Reader
import Data.Foldable qualified as F
import Data.List.NonEmpty qualified as NE
import Data.Morpheus.Types
import Data.Typeable
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import GHC.OverloadedLabels ()
import Melo.Common.Config
import Melo.Common.Monad
import Melo.Format qualified as F
import Melo.Metadata.Aggregate
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Types
import Melo.Metadata.Types

data MetadataQuery m = MetadataQuery
  { formats :: m (Vector (MetadataFormat m)),
    mappings :: m (Vector TagMapping)
  }
  deriving (Generic)

instance Typeable m => GQLType (MetadataQuery m)

data MetadataFormat m = MetadataFormat
  { id :: Text,
    description :: Text,
    mappings :: m (Vector TagMapping)
  }
  deriving (Generic, GQLType)

resolveMetadata ::
  ( TagMappingAggregate m,
    MonadConc m,
    ConfigService m
  ) =>
  ResolverQ e m MetadataQuery
resolveMetadata =
  lift $
    pure
      MetadataQuery
        { mappings = lift do
            allMappings <- V.fromList . itoListOf itraversed <$> getAllMappings
            pure $ fmap (uncurry fromTagMapping) allMappings,
          formats = do
            config <- lift $ getConfigDefault metadataConfigKey
            let allowed f = f.metadataFormat.formatId `F.elem` config.tagPreference
            getMappings <- lift $ once do
              allMappings <- V.fromList . itoListOf itraversed <$> getAllMappings
              pure $ fmap (uncurry fromTagMapping) allMappings
            pure $
              V.fromList $
                mfilter allowed factories <&> \f ->
                  MetadataFormat
                    { id = f.metadataFormat.formatId.unMetadataId,
                      description = f.metadataFormat.formatDesc,
                      mappings = do
                        allMappings <- lift $ getMappings
                        let !id = f.metadataFormat.formatId.unMetadataId
                        pure $
                          allMappings <&> \mapping ->
                            mapping
                              { fieldMappings =
                                  NE.fromList $
                                    NE.filter (\fm -> fm.formatId == id) mapping.fieldMappings
                              }
                    }
        }
