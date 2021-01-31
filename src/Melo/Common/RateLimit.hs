{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.RateLimit where

import Control.Concurrent.TokenLimiter
import Control.Monad.IO.Class
import Control.Monad.Reader

class Monad m => RateLimit m where
  waitReady :: m ()

newtype RateLimitT m a = RateLimitT
  { runRateLimitT :: ReaderT (LimitConfig, RateLimiter) m a
  }
  deriving newtype (Applicative, Functor, Monad, MonadIO, MonadTrans)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    RateLimit m
  ) =>
  RateLimit (t m)
  where
  waitReady = lift waitReady

instance MonadIO m => RateLimit (RateLimitT m) where
  waitReady = RateLimitT $ do
    (lc, rl) <- ask
    liftIO $ waitDebit lc rl 1

runRateLimitIO ::
  MonadIO m =>
  LimitConfig ->
  RateLimitT m a ->
  m a
runRateLimitIO lc c = do
  rl <- liftIO $ newRateLimiter lc
  runRateLimitIO' lc rl c

runRateLimitIO' ::
  LimitConfig ->
  RateLimiter ->
  RateLimitT m a ->
  m a
runRateLimitIO' lc rl c = runReaderT (runRateLimitT c) (lc, rl)
