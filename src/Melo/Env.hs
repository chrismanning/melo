{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}

module Melo.Env
  ( initEnv,
    Env (..),
    EnvVar(..),
    Server (..),
    Database (..),
    DatabaseConnectionPool (..),
    LoggingConfig (..),
    LokiConfig (..),
  )
where

import Data.ByteString.Char8 qualified as C8
import Data.Either.Combinators
import Data.Maybe
import Data.Time.Clock
import Data.Word
import GHC.Exts
import GHC.Generics
import GHC.TypeLits
import System.Environment (lookupEnv)
import System.Exit
import Text.Casing
import Text.Read

initEnv :: IO Env
initEnv = getEnv @Env

data Env = Env
  { server :: Server,
    database :: Database,
    logging :: LoggingConfig
  }
  deriving (Show, Generic)

instance GetEnv Env

data Server = Server
  { port :: EnvVar Int 5000
  }
  deriving (Show, Generic)

data Database = Database
  { host :: EnvVar C8.ByteString "localhost",
    port :: EnvVar Word16 5432,
    user :: EnvVar C8.ByteString "melo",
    password :: EnvVar C8.ByteString "melo",
    database :: EnvVar C8.ByteString "melo",
    pool :: DatabaseConnectionPool
  }
  deriving (Show, Generic)

data DatabaseConnectionPool = DatabaseConnectionPool
  { maxIdleTime :: EnvVar NominalDiffTime 20,
    maxConnections :: EnvVar Int 10
  }
  deriving (Show, Generic)

data LoggingConfig = Logging
  { loki :: LokiConfig
  }
  deriving (Show, Generic)

data LokiConfig = LokiConfig
  { url :: Maybe String
  , bufferSize :: Maybe Natural
  }
  deriving (Show, Generic)

class GetEnv a where
  getEnv :: IO a
  default getEnv :: (Generic a, GetEnv' (Rep a)) => IO a
  getEnv = to <$> getEnv' Nothing

class EnvValue a where
  parseEnv :: String -> Either String a

parseEnvMaybe :: EnvValue a => String -> Maybe a
parseEnvMaybe = rightToMaybe . parseEnv

instance {-# OVERLAPPABLE #-} Read a => EnvValue a where
  parseEnv = readEither

instance EnvValue String where
  parseEnv = Right

instance EnvValue C8.ByteString where
  parseEnv = Right . C8.pack

class GetEnv' f where
  getEnv' :: Maybe String -> IO (f a)

instance GetEnv' f => GetEnv' (D1 m f) where
  getEnv' prefix = M1 <$> getEnv' @f prefix

instance TypeError (Text "Cannot handle multiple contructors") => GetEnv' (x :+: y) where
  getEnv' _ = error "unreachable"

instance TypeError (Text "Cannot handle non-record types") => GetEnv' (C1 ('MetaCons x y 'False) f) where
  getEnv' _ = error "unreachable"

instance TypeError (Text "Fields must be named") => GetEnv' (C1 ('MetaCons x y 'True) (S1 ('MetaSel 'Nothing u s l) k)) where
  getEnv' _ = error "unreachable"

instance GetEnv' s => GetEnv' (C1 ('MetaCons x y 'True) s) where
  getEnv' prefix = M1 <$> getEnv' @s prefix

instance (GetEnv' s, GetEnv' ss) => GetEnv' (s :*: ss) where
  getEnv' prefix = do
    f <- getEnv' @s prefix
    fs <- getEnv' @ss prefix
    pure $ f :*: fs

instance (KnownSymbol n, KnownNat def, EnvValue a, Num a) => GetEnv' (S1 ('MetaSel ('Just n) u s l) (Rec0 (EnvVar a def))) where
  getEnv' prefix = do
    let envVarName = getEnvVarName @n prefix
    lookupEnv envVarName >>= \case
      Just val -> case parseEnv val of
        Right val -> pure (M1 (K1 (EnvVar val)))
        Left e -> die $ "failed to parse env var " <> envVarName <> " value " <> show val <> ": " <> e
      Nothing -> do
        let def' = fromInteger $ natVal' (proxy# @def)
        pure (M1 (K1 (EnvVar def')))

instance (KnownSymbol n, KnownSymbol def, EnvValue a) => GetEnv' (S1 ('MetaSel ('Just n) u s l) (Rec0 (EnvVar a def))) where
  getEnv' prefix = do
    let envVarName = getEnvVarName @n prefix
    lookupEnv envVarName >>= \case
      Just val -> case parseEnv val of
        Right val -> pure (M1 (K1 (EnvVar val)))
        Left e -> die $ "failed to parse env var " <> envVarName <> " value " <> show val <> ": " <> e
      Nothing -> do
        let def' = symbolVal' (proxy# @def)
        case parseEnv def' of
          Right def' -> pure (M1 (K1 (EnvVar def')))
          Left e -> error $ "unable to parse default value for env var " <> envVarName <> ": " <> show def' <> ": " <> show e

instance (KnownSymbol n, EnvValue a) => GetEnv' (S1 ('MetaSel ('Just n) u s l) (Rec0 (Maybe a))) where
  getEnv' prefix = do
    let envVarName = getEnvVarName @n prefix
    !val <- fmap (>>= parseEnvMaybe) $ lookupEnv envVarName
    pure (M1 (K1 val))

instance {-# OVERLAPPABLE #-} (KnownSymbol n, Generic a, GetEnv' (Rep a)) => GetEnv' (S1 ('MetaSel ('Just n) u s l) (Rec0 a)) where
  getEnv' prefix = M1 . K1 . to <$> getEnv' @(Rep a) (Just (getEnvVarName @n prefix))

getEnvVarName :: forall n. KnownSymbol n => Maybe String -> String
getEnvVarName prefix = fromMaybe "" (fmap (<> "_") prefix) <> toScreamingSnake (fromAny $ symbolVal' (proxy# @n))

newtype EnvVar a d = EnvVar { unwrap :: a }
  deriving newtype (Show)
