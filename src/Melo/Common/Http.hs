{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Http where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Monad.IO.Class
import Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Data.Functor
import Data.String (IsString)
import Data.Text as T
import Melo.Common.Effect
import Melo.Common.Logging
import Network.HTTP.Client
import Network.HTTP.Types
import qualified Network.Wreq as Wr
import qualified Network.Wreq.Session as WrS

data Http :: Effect where
  GetWith :: Wr.Options -> Text -> Http m (Response L.ByteString)

getWith :: (Has Http sig m) => Wr.Options -> Text -> m (Response L.ByteString)
getWith opts url = send (GetWith opts url)

getWithJson ::
  ( FromJSON a,
    Has Http sig m,
    Has (Lift IO) sig m
  ) =>
  Wr.Options ->
  Text ->
  m (Response a)
getWithJson opts url = do
  r <- getWith opts url
  sendM @IO $ Wr.asJSON r

newtype HttpIOC m a
  = HttpIOC
      { runHttpIOC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

newtype HttpSessionIOC m a
  = HttpSessionIOC
      { runHttpSessionIOC :: m a
      }
  deriving newtype (Applicative, Functor, Monad, MonadIO)

instance
  ( Has (Lift IO) sig m,
    Has (Reader WrS.Session) sig m,
    Has Logging sig m,
    Algebra sig m
  ) =>
  Algebra (Http :+: sig) (HttpSessionIOC m)
  where
  alg _ (L http) ctx = do
    sess <- ask @WrS.Session
    HttpSessionIOC $
      (ctx $>) <$> case http of
        GetWith opts url -> do
          $(logDebugShow) opts
          r <- sendM $ WrS.getWith opts sess (T.unpack url)
          $(logDebugShow) r
          pure r
  alg hdl (R other) ctx = HttpSessionIOC $ alg (runHttpSessionIOC . hdl) other ctx

runHttpSession :: WrS.Session -> HttpSessionIOC (ReaderC WrS.Session m) a -> m a
runHttpSession sess = runReader sess . runHttpSessionIOC

meloUserAgent :: IsString s => s
meloUserAgent = "melo/0.1.0.0 ( https://github.com/chrismanning/melo )"
