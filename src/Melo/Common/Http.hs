{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Http
  ( Http (..),
    HttpSessionIOT (..),
    runNewHttpSession,
    runHttpSession,
    meloUserAgent,
    HttpClientException (..),
  )
where

import Control.Applicative
import Control.Concurrent.Classy (MonadConc)
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
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
import Witch

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

newtype HttpSessionIOT m a = HttpSessionIOT
  { runHttpSessionIOT :: ReaderT WrS.Session m a
  }
  deriving newtype (
    Applicative,
    Functor,
    Monad,
    MonadBase b,
    MonadBaseControl b,
    MonadCatch,
    MonadConc,
    MonadIO,
    MonadMask,
    MonadParallel,
    MonadThrow,
    MonadTrans
  )

instance
  ( MonadIO m,
    Logging m
  ) =>
  Http (HttpSessionIOT m)
  where
  getWith opts url = HttpSessionIOT $
    ReaderT $ \sess -> do
      $(logDebugShow) url
      $(logDebugShow) opts
      r <- liftIO $ tryAny (WrS.getWith opts sess (T.unpack url))
      $(logDebugShow) r
      pure $ mapLeft (into @HttpClientException) r
  getWithJson opts url = do
    r <- getWith opts url
    $(logDebugShow) r
    case r of
      Left e -> pure $ Left (into @HttpClientException e)
      Right r -> pure $ mapLeft (into @HttpClientException) (Wr.asJSON r)

runNewHttpSession :: MonadIO m => HttpSessionIOT m a -> m a
runNewHttpSession m = do
  sess <- liftIO WrS.newSession
  runReaderT (runHttpSessionIOT m) sess

runHttpSession :: WrS.Session -> HttpSessionIOT m a -> m a
runHttpSession sess m = runReaderT (runHttpSessionIOT m) sess

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"
