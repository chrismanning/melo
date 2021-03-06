{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Http
  ( Http (..),
    HttpSessionT (..),
    runNewHttpSession,
    runHttpSession,
    meloUserAgent,
    HttpClientException (..),
  )
where

import Basement.From
import Control.Applicative
import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Data.Either.Combinators
import Data.Maybe
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Melo.Common.Logging
import Network.HTTP.Client
import qualified Network.Wreq as Wr
import qualified Network.Wreq.Session as WrS

class Monad m => Http m where
  getWith :: Wr.Options -> Text -> m (Either HttpClientException (Response L.ByteString))
  getWithJson :: (Show a, FromJSON a) => Wr.Options -> Text -> m (Either HttpClientException (Response a))

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    Http m
  ) =>
  Http (t m)
  where
  getWith o = lift . getWith o
  getWithJson o = lift . getWithJson o

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

newtype HttpSessionT m a = HttpSessionT
  { runHttpSessionT :: ReaderT WrS.Session m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance
  ( MonadIO m,
    Logging m
  ) =>
  Http (HttpSessionT m)
  where
  getWith opts url = HttpSessionT $
    ReaderT $ \sess -> do
      $(logDebugShowIO) url
      $(logDebugShowIO) opts
      r <- liftIO $ catchAny (Right <$> WrS.getWith opts sess (T.unpack url)) (pure . Left)
      $(logDebugShowIO) r
      pure $ mapLeft (into @HttpClientException) r
  getWithJson opts url = do
    r <- getWith opts url
    $(logDebugShowIO) r
    case r of
      Left e -> pure $ Left (into @HttpClientException e)
      Right r -> pure $ mapLeft (into @HttpClientException) (Wr.asJSON r)

runNewHttpSession :: MonadIO m => HttpSessionT m a -> m a
runNewHttpSession m = do
  sess <- liftIO WrS.newSession
  runReaderT (runHttpSessionT m) sess

runHttpSession :: WrS.Session -> HttpSessionT m a -> m a
runHttpSession sess m = runReaderT (runHttpSessionT m) sess

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"
