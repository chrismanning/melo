{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Tracing
  ( Span,
    SpanArguments (..),
    SpanContext,
    SpanKind (..),
    SpanOperations (..),
    Tracing (..),
    withSpan,
    withSpan',
    withGlobalTracer,
    instrumentationLibrary,
    defaultSpanArguments,
    getSpanContext,
  )
where

import GHC.Stack
import Melo.Common.Exception
import Melo.Common.Monad
import OpenTelemetry.Context qualified as Otel
import OpenTelemetry.Context.ThreadLocal qualified as Otel
import OpenTelemetry.Trace
  ( ImmutableSpan (..),
    InstrumentationLibrary (..),
    Span,
    SpanArguments (..),
    SpanKind (..),
    Tracer,
    defaultSpanArguments,
  )
import OpenTelemetry.Trace qualified as Otel
import OpenTelemetry.Trace.Core qualified as Otel
import System.Environment (lookupEnv)

type SpanContext = Otel.Context

getSpanContext :: (MonadIO m) => m SpanContext
getSpanContext = Otel.getContext

class (Monad m) => SpanOperations m where
  createSpan :: (HasCallStack) => Text -> SpanArguments -> m Span
  endSpan :: Span -> m ()

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    SpanOperations m
  ) =>
  SpanOperations (t m)
  where
  createSpan n args = lift $ createSpan n args
  endSpan = lift . endSpan

class Tracing m where
  withSpan'' :: (HasCallStack) => Text -> SpanArguments -> (Span -> m a) -> m a

withSpan :: (HasCallStack, Tracing m) => Text -> SpanArguments -> m a -> m a
withSpan n a m = withSpan'' n a (const m)

withSpan' :: (HasCallStack, Tracing m) => Text -> SpanArguments -> (Span -> m a) -> m a
withSpan' = withSpan''

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTransControl t,
    Monad m,
    Tracing m
  ) =>
  Tracing (t m)
  where
  withSpan'' n a m = liftWith (\run -> withSpan'' n a (run . m)) >>= restoreT . pure

instrumentationLibrary :: InstrumentationLibrary
instrumentationLibrary =
  InstrumentationLibrary
    { libraryName = "melo",
      libraryVersion = "0.1.0.0"
    }

withGlobalTracer :: (MonadIO m, MonadMask m) => m a -> m a
withGlobalTracer m = ifM
  (liftIO $ lookupEnv "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" <&> isn't _Nothing)
  do
    bracket
      (liftIO Otel.initializeGlobalTracerProvider)
      Otel.shutdownTracerProvider
      $ const m
  do
    m

newtype TracerWrapper = TracerWrapper Tracer
  deriving (Typeable)

getTracer :: (MonadIO m, AppDataReader m) => m Tracer
getTracer =
  getAppData @TracerWrapper >>= \case
    Just (TracerWrapper tracer) -> pure tracer
    Nothing -> do
      tracerProvider <- Otel.getGlobalTracerProvider
      let !tracer = Otel.makeTracer tracerProvider instrumentationLibrary Otel.tracerOptions
      putAppData (TracerWrapper tracer)
      pure tracer

instance (MonadIO m) => SpanOperations (AppM IO m) where
  createSpan n args = do
    tracer <- getTracer
    ctx <- getSpanContext
    s <- Otel.createSpan tracer ctx n args
    Otel.adjustContext (Otel.insertSpan s)
    pure s
  endSpan s = do
    span <- Otel.unsafeReadSpan s
    Otel.adjustContext \ctx ->
      maybe (Otel.removeSpan ctx) (`Otel.insertSpan` ctx) span.spanParent
    Otel.endSpan s Nothing

instance Tracing (AppM IO IO) where
  withSpan'' n a m = do
    tracer <- getTracer
    Otel.inSpan'' tracer n a m
