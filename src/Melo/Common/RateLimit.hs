{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.RateLimit where

import Control.Algebra
import Control.Carrier.Reader
import Control.Concurrent.TokenLimiter
import Control.Effect.Lift
import Melo.Common.Effect

waitReady :: Has RateLimit sig m => m ()
waitReady = send WaitReady

data RateLimit :: Effect where
  WaitReady :: RateLimit m ()

newtype RateLimitIOC m a = RateLimitIOC
  { runRateLimitIOC :: (ReaderC LimitConfig (ReaderC RateLimiter m)) a
  }
  deriving newtype (Applicative, Functor, Monad)

instance
  ( Has (Lift IO) sig m,
    Algebra sig m
  ) =>
  Algebra (RateLimit :+: sig) (RateLimitIOC m)
  where
  alg hdl sig ctx = case sig of
    L WaitReady -> RateLimitIOC $ do
      lc <- ask
      rl <- ask
      sendM $ waitDebit lc rl 1
      pure ctx
    R other ->
      RateLimitIOC $
        ReaderC $
          \lc ->
            ReaderC $ \rl ->
              alg (runReader rl . runReader lc . runRateLimitIOC . hdl) other ctx

runRateLimitIO ::
  Has (Lift IO) sig m =>
  LimitConfig ->
  RateLimitIOC m a ->
  m a
runRateLimitIO lc c = do
  rl <- sendM $ newRateLimiter lc
  runReader rl $ runReader lc $ runRateLimitIOC c
