module Melo.Common.Monad (
  module Control.Monad,
  module Control.Monad.Base,
  module Control.Monad.Conc.Class,
  module Control.Monad.Primitive,
  module Control.Monad.Reader.Class,
  module Control.Monad.Trans,
  module Control.Monad.Trans.Control,
  module Control.Monad.Trans.Identity,
  module Control.Monad.Trans.Maybe,
  module Control.Monad.Trans.Reader,
  forMaybeM,
  (<<|>>),
) where

import Control.Applicative as A
import Control.Monad.Primitive
import Control.Monad
import Control.Monad.Base
import Control.Monad.Conc.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Maybe hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Vector qualified as V

forMaybeM :: Monad m => V.Vector a -> (a -> m (Maybe b)) -> m (V.Vector b)
forMaybeM = flip V.mapMaybeM

(<<|>>) :: (Monad m, Alternative f, Eq (f a)) => m (f a) -> m (f a) -> m (f a)
a <<|>> b =
  a >>= \case
    a'
      | a' == A.empty ->
          b >>= \case
            b' | b' == A.empty -> pure A.empty
            b' -> pure b'
    a' -> pure a'
