module Melo.Common.Concurrent where

import Control.Algebra
import Control.Carrier.Lift
import qualified Control.Concurrent as C
import Control.Monad

forkIO :: Has (Lift IO) sig m => m a -> m C.ThreadId
forkIO m =
  liftWith
    (\run ctx -> (<$ ctx) <$> C.forkIO (void (run (m <$ ctx))))
