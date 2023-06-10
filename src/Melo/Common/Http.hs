module Melo.Common.Http where

import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Strict
import Data.Aeson qualified as A
import Data.ByteString.Char8 as C8
import Data.ByteString.Streaming.HTTP
import Data.ByteString.Streaming.Aeson (decode)
import Data.String (IsString)
import Melo.Common.Exception
import Melo.Common.Logging
import Network.HTTP.Client as Http
import Network.HTTP.Types.Status

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"

catchHttp :: MonadCatch m => m a -> (HttpExceptionContent -> m a) -> m a
catchHttp = flip handleHttp

handleHttp :: MonadCatch m => (HttpExceptionContent -> m a) -> m a -> m a
handleHttp handler = handle \case
  HttpExceptionRequest _ e -> handler e
  e@InvalidUrlException {} -> throwM e

httpJson ::
  forall a m.
  ( A.FromJSON a,
    MonadCatch m,
    MonadIO m,
    MonadReader Http.Manager m
  ) =>
  Http.Request ->
  m (Maybe a)
httpJson req = handle' do
  manager <- ask
  let request = req {requestHeaders = [("User-Agent", meloUserAgent), ("Accept", "application/json")]}
  liftIO $ withHTTP request manager \response -> do
    let clientResponseStatus = show response.responseStatus
    $(logDebugV ['clientRequestUrl, 'clientResponseStatus]) "HTTP client received response"
    if response.responseStatus == status200
      then
        evalStateT decode response.responseBody >>= \case
          Left e -> do
            let cause = displayException e
            $(logErrorVIO ['clientRequestUrl, 'cause]) "HTTP client error"
            pure Nothing
          Right a -> pure $ Just a
      else pure Nothing
    where
      handle' = handleAny \e -> do
        let cause = displayException e
        $(logErrorVIO ['clientRequestUrl, 'cause]) "HTTP client error"
        pure Nothing
      clientRequestUrl = C8.unpack $ req.host <> ":" <> C8.pack (show req.port) <> req.path <> req.queryString
