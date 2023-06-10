{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.RateLimit where

import Control.Concurrent.Classy
import Control.Concurrent.TokenLimiter
import Control.Monad.Primitive
import Melo.Common.Exception
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control

class Monad m => RateLimit m where
  waitReady :: m ()

newtype RateLimitIOT m a = RateLimitIOT
  { runRateLimitIOT :: ReaderT (Maybe (LimitConfig, RateLimiter)) m a
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
    MonadThrow,
    MonadTrans,
    MonadTransControl,
    PrimMonad
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
  waitReady = RateLimitIOT
    ask >>= \case
      Just (lc, rl) ->
        liftIO $ waitDebit lc rl 1
      Nothing -> pure ()

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
runRateLimitIO' lc rl c = runReaderT (runRateLimitIOT c) (Just (lc, rl))

runDynamicRateLimitIO ::
  MonadIO m =>
  Maybe LimitConfig ->
  RateLimitIOT m a ->
  m a
runDynamicRateLimitIO (Just lc) c = runRateLimitIO lc c
runDynamicRateLimitIO Nothing c = runReaderT (runRateLimitIOT c) Nothing
