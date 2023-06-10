module Melo.Common.Http where

import Data.String (IsString)
import Melo.Common.Exception
import Network.HTTP.Client

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"

catchHttp :: MonadCatch m => m a -> (HttpExceptionContent -> m a) -> m a
catchHttp = flip handleHttp

handleHttp :: MonadCatch m => (HttpExceptionContent -> m a) -> m a -> m a
handleHttp handler = handle \case
  HttpExceptionRequest _ e -> handler e
  e@InvalidUrlException {} -> throwM e
