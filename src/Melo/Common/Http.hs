{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Http
  ( Http (..),
    getWith,
    getWithJson,
    HttpIOC (..),
    HttpSessionIOC (..),
    runNewHttpSession,
    runHttpSession,
    meloUserAgent,
    HttpClientException (..),
    runHttpError,
    runHttpEither,
    runHttpAlt,
    runHttpEmpty,
  )
where

import Basement.From
import Control.Algebra
import Control.Applicative
import qualified Control.Carrier.Empty.Church as E
import Control.Carrier.Error.Church
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Exception.Safe
import Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Data.Functor
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Melo.Common.Effect
import Melo.Common.Logging
import Network.HTTP.Client
import qualified Network.Wreq as Wr
import qualified Network.Wreq.Session as WrS

data Http :: Effect where
  GetWith :: Wr.Options -> Text -> Http m (Response L.ByteString)
  GetWithJson :: (Show a, FromJSON a) => Wr.Options -> Text -> Http m (Response a)

getWith ::
  Has Http sig m =>
  Wr.Options ->
  Text ->
  m (Response L.ByteString)
getWith opts url = send (GetWith opts url)

getWithJson ::
  (Show a, FromJSON a, Has Http sig m) =>
  Wr.Options ->
  Text ->
  m (Response a)
getWithJson opts url = send (GetWithJson opts url)

data HttpClientException
  = HttpClientException !HttpException
  | JsonException !Wr.JSONError
  | UnknownHttpError !SomeException
  deriving (Show)

instance Exception HttpClientException

instance From SomeException HttpClientException where
  from e =
    fromMaybe (UnknownHttpError e) $
      HttpClientException <$> fromException @HttpException e
        <|> JsonException <$> fromException @Wr.JSONError e

runHttpError :: (HttpClientException -> m b) -> (a -> m b) -> ErrorC HttpClientException m a -> m b
runHttpError = runError

runHttpEither :: Applicative m => ErrorC HttpClientException m a -> m (Either HttpClientException a)
runHttpEither = runHttpError (pure . Left) (pure . Right)

runHttpAlt :: (Has Http sig m, Has Logging sig m, Alternative f) => ErrorC HttpClientException m (f a) -> m (f a)
runHttpAlt = runHttpError (\e -> $(logErrorShow) e >> pure empty) pure

runHttpEmpty :: (Has Logging sig m, Has E.Empty sig m) => ErrorC HttpClientException m a -> m a
runHttpEmpty = runHttpError (\e -> $(logErrorShow) e >> E.empty) pure

newtype HttpIOC m a = HttpIOC
  { runHttpIOC :: m a
  }
  deriving newtype (Applicative, Functor, Monad)

newtype HttpSessionIOC m a = HttpSessionIOC
  { runHttpSessionIOC :: ReaderC WrS.Session (ErrorC HttpClientException m) a
  }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has (Lift IO) sig m,
    Has Logging sig m
  ) =>
  Algebra (Http :+: sig) (HttpSessionIOC m)
  where
  alg _ (L http) ctx =
    case http of
      GetWith opts url -> HttpSessionIOC $ do
        $(logDebugShow) url
        $(logDebugShow) opts
        sess <- ask @WrS.Session
        r <- sendM $ catchAny (Right <$> WrS.getWith opts sess (T.unpack url)) (pure . Left)
        case r of
          Left e -> throwError (into @HttpClientException e)
          Right r' -> do
            $(logDebugShow) r'
            pure $ ctx $> r'
      GetWithJson opts url -> do
        r <- getWith opts url
        case Wr.asJSON r of
          Left e -> HttpSessionIOC $ ReaderC $ \_ -> throwError (into @HttpClientException e)
          Right r' -> do
            $(logDebugShow) r'
            pure $ ctx $> r'
  alg hdl (R other) ctx = HttpSessionIOC $ alg (runHttpSessionIOC . hdl) (R (R other)) ctx

runNewHttpSession :: Has (Lift IO) sig m => HttpSessionIOC m a -> ErrorC HttpClientException m a
runNewHttpSession m = do
  sess <- sendM WrS.newSession
  runReader sess $ runHttpSessionIOC m

runHttpSession :: WrS.Session -> HttpSessionIOC m a -> ErrorC HttpClientException m a
runHttpSession sess = runReader sess . runHttpSessionIOC

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"
