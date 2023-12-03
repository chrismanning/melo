{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Config where

import Data.Aeson qualified as A
import Data.Default
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.Text qualified as T
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Rel8

data ConfigTable f = ConfigTable
  { key :: Column f Text,
    value :: Column f (JSONBEncoded ConfigValue)
  }
  deriving (Generic, Rel8able)

type ConfigEntity = ConfigTable Result

deriving via FromGeneric ConfigEntity instance TextShow ConfigEntity

instance Entity ConfigEntity where
  type NewEntity ConfigEntity = ConfigEntity
  type PrimaryKey ConfigEntity = Text
  primaryKey e = e.key

instance From ConfigEntity (ConfigTable Expr) where
  from config =
    ConfigTable
      { key = lit config.key,
        value = lit config.value
      }

newtype ConfigKey v = ConfigKey
  { key :: Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Hashable, A.ToJSON)
  deriving (TextShow) via FromGeneric (ConfigKey v)

type ConfigValue = A.Value

class Monad m => ConfigService m where
-- TODO invalidateCache :: m ()
  getConfig :: A.FromJSON v => ConfigKey v -> m (Maybe v)
  setConfig :: A.ToJSON v => ConfigKey v -> v -> m ()
  updateConfig :: A.ToJSON v => ConfigKey v -> v -> m ()

getConfigDefault :: (A.FromJSON v, Default v, ConfigService m) => ConfigKey v -> m v
getConfigDefault k = fromMaybe def <$> getConfig k

getConfig' :: (A.FromJSON v, ConfigService m) => ConfigKey v -> v -> m v
getConfig' k d = fromMaybe d <$> getConfig k

type ConfigRepository = Repo.Repository ConfigEntity

newtype ConfigCache = ConfigCache {configs :: HashMap Text ConfigEntity}
  deriving (Generic, Typeable)
  deriving newtype (TextShow)

getConfigCache :: (AppDataReader m, ConfigRepository m) => m ConfigCache
getConfigCache = getAppData @ConfigCache >>= \case
  Just config -> pure config
  Nothing -> do
    all <- toList <$> getAll @ConfigEntity
    let !config = ConfigCache $ HashMap.fromList $ fmap (\c -> (c.key, c)) all
    putAppData config
    pure config

instance
  ( Logging m,
    Monad m,
    AppDataReader m,
    ConfigRepository m,
    MonadCatch m
  ) =>
  ConfigService m
  where
  getConfig configKey@ConfigKey {key} =
    getConfigCache <&> HashMap.lookup key . (.configs) >>= \case
      Nothing -> pure Nothing
      Just e -> case A.fromJSON e.value.fromJSONBEncoded of
        A.Success v -> pure $ Just v
        A.Error error -> do
          $(logErrorV ['configKey]) $ "Unable to parse config value: " <> T.pack error
          pure Nothing
  setConfig ConfigKey {key} v =
    let e = ConfigTable {key, value = JSONBEncoded (A.toJSON v)}
     in handleAny (const (pure ())) (Repo.insertSingle' @ConfigEntity e >> deleteAppData @ConfigCache)
  updateConfig configKey@ConfigKey {key} v =
    let e = ConfigTable {key, value = JSONBEncoded (A.toJSON v)}
     in catchAny (Repo.updateSingle' @ConfigEntity e >> deleteAppData @ConfigCache) \_error -> do
          $(logWarnV ['configKey]) "Could not update config value; creating new value"
          Repo.insertSingle' @ConfigEntity e

configSchema :: TableSchema (ConfigTable Name)
configSchema =
  TableSchema
    { name = "config",
      columns =
        ConfigTable
          { key = "key",
            value = "value"
          }
    }

initConfigRepo :: AppDataReader m => m ()
initConfigRepo = putAppData
  RepositoryHandle
    { tbl = configSchema,
      pk = (.key),
      upsert = Nothing
    }
