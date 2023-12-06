module Melo.Metadata.API where

import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import Melo.Common.API
import Melo.Common.Config
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Metadata.Aggregate
import Melo.Metadata.Mapping.Aggregate
import Melo.Metadata.Mapping.Types
import Melo.Metadata.Types

data MetadataFormat = MetadataFormat
  { id :: Text,
    description :: Text,
    mappings :: Vector TagMapping
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions MetadataFormat

getMetadataTagMappings :: AppM IO IO (Vector TagMapping)
getMetadataTagMappings = do
  allMappings <- V.fromList . itoListOf itraversed <$> getAllMappings
  pure $ fmap (uncurry fromTagMapping) allMappings

getMetadataFormats :: AppM IO IO (Vector MetadataFormat)
getMetadataFormats = do
  config <- getConfigDefault metadataConfigKey
  let allowed f = f ^. #metadataFormat . #formatId `elem` config.tagPreference
  allMappings <- getMetadataTagMappings
  pure $
    V.fromList $
      mfilter allowed factories <&> \f -> do
        MetadataFormat
          { id = formatId f,
            description = formatDesc f,
            mappings = mappings f allMappings
          }
  where
    formatId f = f ^. #metadataFormat . #formatId . #unMetadataId
    formatDesc f = f ^. #metadataFormat . #formatDesc
    mappings :: MetadataFormatFactory -> Vector TagMapping -> Vector TagMapping
    mappings f allMappings = allMappings
      & filter (\m -> any (\fm -> fm.formatId == formatId f) m.fieldMappings)
      <&> (#fieldMappings %~ NE.fromList . filter (\fm -> fm.formatId == formatId f) . toList)

registerRoutes :: AppM IO IO ()
registerRoutes = do
  registerRoute (RouteKey "getMetadataFormats") (nullRqJsonRsRoute getMetadataFormats)
  registerRoute (RouteKey "getMetadataTagMappings") (nullRqJsonRsRoute getMetadataTagMappings)
  pure ()
