{-# LANGUAGE UndecidableInstances #-}

module Melo.Common.Routing where

import Control.Monad.Class.MonadThrow
import Control.Monad.Cont
import Data.Aeson qualified as JSON
import Data.HashMap.Strict (HashMap)
import Data.Text as T
import Data.Typeable
import Melo.Common.API
import Melo.Common.Logging
import Melo.Common.Monad
import Network.RSocket as RSocket
import Streaming.Prelude qualified as S

data Router m = Router
  { fireNForgetRoutes :: HashMap RouteKey (RouteHandler RSocket.RequestFireNForget m),
    singleResponseRoutes :: HashMap RouteKey (RouteHandler RSocket.RequestResponse m),
    streamResponseRoutes :: HashMap RouteKey (RouteHandler RSocket.RequestStream m),
    channelRoutes :: HashMap RouteKey (RouteHandler RSocket.RequestChannel m)
  }
  deriving (Generic, Typeable)

mkRouter :: Router m
mkRouter = Router mempty mempty mempty mempty

getRouter :: forall m. (AppDataReader m, Typeable m) => m (Router m)
getRouter =
  getAppData @(Router m) >>= \case
    Just router -> pure router
    Nothing -> do
      let (!router :: Router m) = mkRouter
      putAppData router
      pure router

data TypedRequest = TypedRequest
  { streamId :: RSocket.StreamId,
    requestMimeType :: RSocket.MimeType,
    requestPayload :: RSocket.DataPayload
  }

class RouteRegister rq m where
  type RouteHandler rq m = h | h -> rq m
  type RouteResult rq m = r | r -> rq
  registerRoute :: RouteKey -> RouteHandler rq m -> m ()
  handleRoute :: RouteKey -> RSocket.ErasedConnection m -> rq -> RSocket.MimeType -> RSocket.Metadata -> RouteResult rq m

instance (AppDataReader m, Logging m, MonadThrow m, Typeable m) => RouteRegister RSocket.RequestFireNForget m where
  type RouteHandler RSocket.RequestFireNForget m = RSocket.ErasedConnection m -> TypedRequest -> m ()
  type RouteResult RSocket.RequestFireNForget m = m ()
  registerRoute routeKey routeHandler = do
    $(logInfo) $ "Registering fire n forget route " <> T.pack (show routeKey)
    alterAppData @(Router m) \r ->
      Just $! fromMaybe mkRouter r & #fireNForgetRoutes . at routeKey ?~ routeHandler
  handleRoute routeKey conn rq dataMime _metadata =
    getRouter >>= \router -> case router.fireNForgetRoutes ^. at routeKey of
      Just routeHandler -> routeHandler conn (TypedRequest rq.streamId dataMime rq.requestPayload)
      Nothing -> do
        $(logError) $ "Unknown fire n forget route " <> T.pack (show routeKey)
        throwIO (UnknownRoute routeKey)

instance (AppDataReader m, Logging m, MonadThrow m, Typeable m) => RouteRegister RSocket.RequestResponse m where
  type RouteHandler RSocket.RequestResponse m = RSocket.ErasedConnection m -> TypedRequest -> [RSocket.MimeType] -> m RSocket.Payload
  type RouteResult RSocket.RequestResponse m = m RSocket.Payload
  registerRoute routeKey routeHandler = do
    $(logInfo) $ "Registering single response route " <> T.pack (show routeKey)
    alterAppData @(Router m) \r ->
      Just $! fromMaybe mkRouter r & #singleResponseRoutes . at routeKey ?~ routeHandler
  handleRoute routeKey conn rq dataMime metadata =
    getRouter >>= \router -> case router.singleResponseRoutes ^. at routeKey of
      Just routeHandler -> routeHandler conn (TypedRequest rq.streamId dataMime rq.requestPayload) (acceptableResponseMimes metadata)
      Nothing -> do
        $(logError) $ "Unknown single response route " <> T.pack (show routeKey)
        throwIO (UnknownRoute routeKey)

instance (AppDataReader m, Logging m, MonadIO m, Typeable m) => RouteRegister RSocket.RequestStream m where
  type RouteHandler RSocket.RequestStream m = RSocket.ErasedConnection m -> TypedRequest -> [RSocket.MimeType] -> ContT () m (S.Stream (S.Of RSocket.Payload) m ())
  type RouteResult RSocket.RequestStream m = ContT () m (S.Stream (S.Of RSocket.Payload) m ())
  registerRoute routeKey routeHandler = do
    $(logInfo) $ "Registering stream route " <> T.pack (show routeKey)
    alterAppData @(Router m) \r ->
      Just $! fromMaybe mkRouter r & #streamResponseRoutes . at routeKey ?~ routeHandler
  handleRoute routeKey conn rq dataMime metadata =
    lift getRouter >>= \router -> case router.streamResponseRoutes ^. at routeKey of
      Just routeHandler -> routeHandler conn (TypedRequest rq.streamId dataMime rq.requestPayload) (acceptableResponseMimes metadata)
      Nothing -> lift do
        $(logError) $ "Unknown stream route " <> T.pack (show routeKey)
        liftIO $ throwIO (UnknownRoute routeKey)

instance (AppDataReader m, Logging m, MonadIO m, Typeable m) => RouteRegister RSocket.RequestChannel m where
  type RouteHandler RSocket.RequestChannel m = RSocket.ErasedConnection m -> TypedRequest -> ContT () m (S.Stream (S.Of RSocket.Payload) m (Maybe RSocket.Payload) -> m (S.Stream (S.Of RSocket.Payload) m ()))
  type RouteResult RSocket.RequestChannel m = ContT () m (S.Stream (S.Of RSocket.Payload) m (Maybe RSocket.Payload) -> m (S.Stream (S.Of RSocket.Payload) m ()))
  registerRoute routeKey routeHandler = do
    $(logInfo) $ "Registering channel route " <> T.pack (show routeKey)
    alterAppData @(Router m) \r ->
      Just $! fromMaybe mkRouter r & #channelRoutes . at routeKey ?~ routeHandler
  handleRoute routeKey conn rq dataMime _metadata =
    lift getRouter >>= \router -> case router.channelRoutes ^. at routeKey of
      Just routeHandler -> routeHandler conn (TypedRequest rq.streamId dataMime rq.requestPayload)
      Nothing -> do
        $(logError) $ "Unknown channel route " <> T.pack (show routeKey)
        liftIO $ throwIO (UnknownRoute routeKey)

acceptableResponseMimes :: RSocket.Metadata -> [RSocket.MimeType]
acceptableResponseMimes (AcceptableDataMimeTypes ms) = normaliseMimeType <$!> ms
acceptableResponseMimes (CompositeMetadata ms) = normaliseMimeType <$!> (ms >>= acceptableResponseMimes)
acceptableResponseMimes _ = []

parseRoute :: RouteTag -> Maybe RouteKey
parseRoute (RouteTag t)
  | "service:" `T.isPrefixOf` t = RouteKey <$!> T.stripPrefix "service:" t
  | otherwise = Nothing

normaliseMimeType :: RSocket.MimeType -> RSocket.MimeType
normaliseMimeType (RawMimeType "application/json") = MimeTypeId ApplicationJson
normaliseMimeType (RawMimeType "text/plain") = MimeTypeId TextPlain
normaliseMimeType m = m

jsonRqRawRsRoute :: FromJSON a => (a -> RSocket.StreamId -> RouteResult RSocket.RequestResponse (AppM IO IO)) -> RouteHandler RSocket.RequestResponse (AppM IO IO)
jsonRqRawRsRoute f _conn (TypedRequest streamId mimeType payload) _mimes =
  case normaliseMimeType mimeType of
    MimeTypeId ApplicationJson ->
      case JSON.eitherDecode' (RSocket.payloadData payload) of
        Right a -> f a streamId
        Left e -> do
          let !msg = "Failed to decode request: " <> showt e
          $(logWarnIO) msg
          liftIO $ throwIO $ ServiceException msg
    _ -> do
      let !msg = "Unsupported request type " <> T.pack (show mimeType)
      $(logWarnIO) msg
      liftIO $ throwIO $ ServiceException msg

jsonRqJsonRsRoute :: (FromJSON rq, ToJSON rs) => (rq -> AppM IO IO rs) -> RouteHandler RSocket.RequestResponse (AppM IO IO)
jsonRqJsonRsRoute f _conn (TypedRequest streamId mimeType payload) _mimes =
  case normaliseMimeType mimeType of
    MimeTypeId ApplicationJson ->
      case JSON.eitherDecode' (RSocket.payloadData payload) of
        Right a -> do
          rs <- f a
          pure $! buildPayload (JsonPayload rs) (RSocket.CompositeMetadata []) streamId True
        Left e -> do
          let !msg = "Failed to decode request: " <> showt e
          $(logWarnIO) msg
          liftIO $ throwIO $ ServiceException msg
    _ -> do
      let !msg = "Unsupported request type " <> T.pack (show mimeType)
      $(logWarnIO) msg
      liftIO $ throwIO $ ServiceException msg

nullRqJsonRsRoute :: ToJSON rs => AppM IO IO rs -> RouteHandler RSocket.RequestResponse (AppM IO IO)
nullRqJsonRsRoute f _conn (TypedRequest streamId mimeType _payload) _mimes =
  case normaliseMimeType mimeType of
    MimeTypeId ApplicationJson -> do
      rs <- f
      pure $! buildPayload (JsonPayload rs) (RSocket.CompositeMetadata []) streamId True
    _ -> do
      let !msg = "Unsupported request type " <> T.pack (show mimeType)
      $(logWarnIO) msg
      liftIO $ throwIO $ ServiceException msg

jsonRqRawStreamRoute :: FromJSON a => (a -> RSocket.StreamId -> RouteResult RSocket.RequestStream (AppM IO IO)) -> RouteHandler RSocket.RequestStream (AppM IO IO)
jsonRqRawStreamRoute f _conn (TypedRequest streamId mimeType payload) _mimes =
  case normaliseMimeType mimeType of
    MimeTypeId ApplicationJson ->
      case JSON.eitherDecode' (RSocket.payloadData payload) of
        Right a -> f a streamId
        Left e -> do
          let !msg = "Failed to decode request: " <> showt e
          $(logWarnIO) msg
          liftIO $ throwIO $ ServiceException msg
    _ -> do
      let !msg = "Unsupported request type " <> T.pack (show mimeType)
      $(logWarnIO) msg
      liftIO $ throwIO $ ServiceException msg

jsonRqJsonStreamRoute :: (FromJSON rq, ToJSON rs) => (rq -> ContT () (AppM IO IO) (S.Stream (S.Of rs) (AppM IO IO) ())) -> RouteHandler RSocket.RequestStream (AppM IO IO)
jsonRqJsonStreamRoute f _conn (TypedRequest streamId mimeType payload) _mimes =
  case normaliseMimeType mimeType of
    MimeTypeId ApplicationJson ->
      case JSON.eitherDecode' (RSocket.payloadData payload) of
        Right a -> do
          s <- f a
          pure $!
            s
              & S.map (\rs -> buildStreamPayload (JsonPayload rs) (RSocket.CompositeMetadata []) streamId)
        Left e -> do
          let !msg = "Failed to decode request: " <> showt e
          $(logWarnIO) msg
          liftIO $ throwIO $ ServiceException msg
    _ -> do
      let !msg = "Unsupported request type " <> T.pack (show mimeType)
      $(logWarnIO) msg
      liftIO $ throwIO $ ServiceException msg
