{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Melo.Common.Exception
  ( module Control.Exception.Safe,
    throwOnLeft,
    throwOnNothing,
  )
where

import Control.Exception.Safe
import Control.Monad.Catch qualified as C
import Control.Monad.Trans
import Streaming.Internal (Stream (..))

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    MonadThrow m
  ) =>
  MonadThrow (t m)
  where
  throwM = lift . C.throwM

instance
  ( MonadCatch m,
    Functor f
  ) =>
  MonadCatch (Stream f m)
  where
  catch s h = loop s
    where
      loop s = case s of
        Return r -> Return r
        Effect m -> Effect $ C.catch (fmap loop m) (pure . h)
        Step g -> Step (fmap loop g)

throwOnLeft :: (MonadThrow m, Exception e) => Either e a -> m a
throwOnLeft (Left e) = throwM e
throwOnLeft (Right a) = pure a

throwOnNothing :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
throwOnNothing e Nothing = throwM e
throwOnNothing _ (Just a) = pure a
