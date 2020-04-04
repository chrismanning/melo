-- From Control.Effect.Exception
module Melo.Common.Exception where

import Control.Algebra hiding (Handler)
import Control.Concurrent (ThreadId)
import Control.Effect.Lift
import qualified Control.Exception as Exc
import Prelude hiding (ioError)

throwIO :: (Exc.Exception e, Has (Lift IO) sig m) => e -> m a
throwIO = sendM . Exc.throwIO

ioError :: Has (Lift IO) sig m => IOError -> m a
ioError = sendM . Exc.ioError

throwTo :: (Exc.Exception e, Has (Lift IO) sig m) => ThreadId -> e -> m ()
throwTo t = sendM . Exc.throwTo t

catch :: (Exc.Exception e, Has (Lift IO) sig m) => m a -> (e -> m a) -> m a
catch m h = liftWith $ \hdl ctx -> hdl (m <$ ctx) `Exc.catch` (hdl . (<$ ctx) . h)

catches :: Has (Lift IO) sig m => m a -> [Handler m a] -> m a
catches m hs = liftWith $ \hdl ctx ->
  Exc.catches (hdl (m <$ ctx)) (map (\(Handler h) -> Exc.Handler (hdl . (<$ ctx) . h)) hs)

data Handler m a
  = forall e. Exc.Exception e => Handler (e -> m a)

deriving instance Functor m => Functor (Handler m)

catchJust ::
  (Exc.Exception e, Has (Lift IO) sig m) =>
  (e -> Maybe b) ->
  m a ->
  (b -> m a) ->
  m a
catchJust p m h = liftWith $ \hdl ctx -> Exc.catchJust p (hdl (m <$ ctx)) (hdl . (<$ ctx) . h)

handle :: (Exc.Exception e, Has (Lift IO) sig m) => (e -> m a) -> m a -> m a
handle = flip catch

handleJust ::
  (Exc.Exception e, Has (Lift IO) sig m) =>
  (e -> Maybe b) ->
  (b -> m a) ->
  m a ->
  m a
handleJust p = flip (catchJust p)

try :: (Exc.Exception e, Has (Lift IO) sig m) => m a -> m (Either e a)
try m = (Right <$> m) `catch` (pure . Left)

tryJust :: (Exc.Exception e, Has (Lift IO) sig m) => (e -> Maybe b) -> m a -> m (Either b a)
tryJust p m = catchJust p (Right <$> m) (pure . Left)

evaluate :: Has (Lift IO) sig m => a -> m a
evaluate = sendM . Exc.evaluate

mask :: Has (Lift IO) sig m => ((forall a. m a -> m a) -> m b) -> m b
mask with = liftWith $ \hdl ctx -> Exc.mask $ \restore ->
  hdl (with (\m -> liftWith $ \hdl' ctx' -> restore (hdl' (m <$ ctx'))) <$ ctx)

mask_ :: Has (Lift IO) sig m => m a -> m a
mask_ m = mask $ const m

uninterruptibleMask :: Has (Lift IO) sig m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask with = liftWith $ \hdl ctx -> Exc.uninterruptibleMask $ \restore ->
  hdl (with (\m -> liftWith $ \hdl' ctx' -> restore (hdl' (m <$ ctx'))) <$ ctx)

uninterruptibleMask_ :: Has (Lift IO) sig m => m a -> m a
uninterruptibleMask_ m = uninterruptibleMask $ const m

getMaskingState :: Has (Lift IO) sig m => m Exc.MaskingState
getMaskingState = sendM Exc.getMaskingState

interruptible :: Has (Lift IO) sig m => m a -> m a
interruptible m = liftWith $ \hdl ctx -> Exc.interruptible (hdl (m <$ ctx))

allowInterrupt :: Has (Lift IO) sig m => m ()
allowInterrupt = sendM Exc.allowInterrupt

bracket ::
  Has (Lift IO) sig m =>
  m a ->
  (a -> m b) ->
  (a -> m c) ->
  m c
bracket acquire release m = mask $ \restore -> do
  a <- acquire
  r <- restore (m a) `onException` release a
  r <$ release a

bracket_ ::
  Has (Lift IO) sig m =>
  m a ->
  m b ->
  m c ->
  m c
bracket_ before after thing = bracket before (const after) (const thing)

bracketOnError ::
  Has (Lift IO) sig m =>
  m a ->
  (a -> m b) ->
  (a -> m c) ->
  m c
bracketOnError acquire release m = mask $ \restore -> do
  a <- acquire
  restore (m a) `onException` release a

finally ::
  Has (Lift IO) sig m =>
  m a ->
  m b ->
  m a
finally m sequel = mask $ \restore -> (restore m `onException` sequel) <* sequel

onException :: Has (Lift IO) sig m => m a -> m b -> m a
onException io what = io `catch` \e -> what >> throwIO (e :: Exc.SomeException)
