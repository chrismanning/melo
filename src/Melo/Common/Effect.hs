module Melo.Common.Effect where

import Control.Monad
import Data.Functor
import Data.Kind (Type)

type Effect = (Type -> Type) -> (Type -> Type)

($$>) :: (Functor f, Functor g) => f x -> g a -> g (f a)
f $$> g = (f $>) <$> g

infixl 4 $$>

($$!>) :: (Functor f, Monad m) => f x -> m a -> m (f a)
f $$!> m = (f $>) <$!> m

infixl 4 $$!>
