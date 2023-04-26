{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Config where

import Data.Aeson qualified as A
import Data.Default
import Data.Hashable
import Data.Maybe
import Data.Pool
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import Hasql.Connection
import Melo.Common.Exception
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Database.Repo as Repo
import Melo.Database.Repo.IO
import Rel8
import Witch

data ConfigTable f = ConfigTable
  { key :: Column f Text,
    value :: Column f (JSONBEncoded ConfigValue)
  }
  deriving (Generic, Rel8able)

type ConfigEntity = ConfigTable Result

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

type ConfigValue = A.Value

class Monad m => ConfigService m where
  getConfig :: A.FromJSON v => ConfigKey v -> m (Maybe v)
  setConfig :: A.ToJSON v => ConfigKey v -> v -> m ()
  updateConfig :: A.ToJSON v => ConfigKey v -> v -> m ()

getConfigDefault :: (A.FromJSON v, Default v, ConfigService m) => ConfigKey v -> m v
getConfigDefault k = fromMaybe def <$> getConfig k

getConfig' :: (A.FromJSON v, ConfigService m) => ConfigKey v -> v -> m v
getConfig' k d = fromMaybe d <$> getConfig k

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    ConfigService m
  ) =>
  ConfigService (t m) where
  getConfig = lift . getConfig
  setConfig k v = lift $ setConfig k v
  updateConfig k v = lift $ updateConfig k v

newtype ConfigRepositoryIOT m a = ConfigRepositoryIOT
  { runConfigRepositoryIOT :: RepositoryIOT ConfigTable m a
  }
  deriving newtype
    ( Applicative,
      Functor,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadConc,
      MonadCatch,
      MonadMask,
      MonadReader (RepositoryHandle ConfigTable),
      MonadThrow,
      MonadTrans,
      MonadTransControl,
      Repository ConfigEntity,
      PrimMonad
    )

instance
  ( Logging m,
    MonadIO m,
    MonadCatch m
  ) =>
  ConfigService (ConfigRepositoryIOT m)
  where
  getConfig configKey@ConfigKey {key} =
    Repo.getSingle @ConfigEntity key >>= \case
      Nothing -> pure Nothing
      Just e -> case A.fromJSON e.value.fromJSONBEncoded of
        A.Success v -> pure $ Just v
        A.Error error -> do
          $(logErrorV ['configKey]) $ "Unable to parse config value: " <> T.pack error
          pure Nothing
  setConfig ConfigKey {key} v = let e = ConfigTable {key, value = JSONBEncoded (A.toJSON v)} in
    handleAny (const (pure ())) $ Repo.insertSingle' @ConfigEntity e
  updateConfig configKey@ConfigKey {key} v = let e = ConfigTable {key, value = JSONBEncoded (A.toJSON v)} in
    catchAny (Repo.updateSingle' @ConfigEntity e) \_error -> do
      $(logWarnV ['configKey]) $ "Could not update config value; creating new value"
      Repo.insertSingle' @ConfigEntity e

configSchema :: TableSchema (ConfigTable Name)
configSchema =
  TableSchema
    { name = "config",
      schema = Nothing,
      columns =
        ConfigTable
          { key = "key",
            value = "value"
          }
    }

runConfigRepositoryPooledIO :: Pool Connection -> ConfigRepositoryIOT m a -> m a
runConfigRepositoryPooledIO pool =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Pooled pool,
        tbl = configSchema,
        pk = (.key),
        upsert = Nothing
      }
    . runRepositoryIOT
    . runConfigRepositoryIOT

runConfigRepositoryIO :: Connection -> ConfigRepositoryIOT m a -> m a
runConfigRepositoryIO conn =
  flip
    runReaderT
    RepositoryHandle
      { connSrc = Single conn,
        tbl = configSchema,
        pk = (.key),
        upsert = Nothing
      }
    . runRepositoryIOT
    . runConfigRepositoryIOT
