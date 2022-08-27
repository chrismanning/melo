{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.RateLimit where

import Control.Concurrent.Classy
import Control.Concurrent.TokenLimiter
import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Parallel (MonadParallel)
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Trans.Resource

class Monad m => RateLimit m where
  waitReady :: m ()

newtype RateLimitIOT m a = RateLimitIOT
  { runRateLimitIOT :: ReaderT (LimitConfig, RateLimiter) m a
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
    MonadTrans,
    MonadUnliftIO
  )

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    RateLimit m
  ) =>
  RateLimit (t m)
  where
  waitReady = lift waitReady

instance MonadIO m => RateLimit (RateLimitIOT m) where
  waitReady = RateLimitIOT $ do
    (lc, rl) <- ask
    liftIO $ waitDebit lc rl 1

runRateLimitIO ::
  MonadIO m =>
  LimitConfig ->
  RateLimitIOT m a ->
  m a
runRateLimitIO lc c = do
  rl <- liftIO $ newRateLimiter lc
  runRateLimitIO' lc rl c

runRateLimitIO' ::
  LimitConfig ->
  RateLimiter ->
  RateLimitIOT m a ->
  m a
runRateLimitIO' lc rl c = runReaderT (runRateLimitIOT c) (lc, rl)
