module Melo.API where

import Control.Monad.Class.MonadThrow
import Control.Monad.Cont
import Control.Monad.IO.Class
import Data.Text qualified as T
import Melo.Common.API
import Melo.Common.Logging
import Melo.Common.Monad
import Melo.Common.Routing
import Melo.Library.Collection.API qualified as CollectionAPI
import Melo.Metadata.API qualified as MetadataAPI
import Melo.Library.Source.API qualified as SourceAPI
import Network.RSocket as RSocket

registerRoutes :: AppM IO IO ()
registerRoutes = do
  CollectionAPI.registerRoutes
  MetadataAPI.registerRoutes
  SourceAPI.registerRoutes
  pure ()

rsocketHandlers :: RSocket.MimeType -> RSocket.MimeType -> InteractionHandlers (ErasedHandle (AppM IO IO)) (AppM IO IO)
rsocketHandlers metadataMimeType dataMimeType =
  InteractionHandlers
    { handleFireNForgetRequest,
      handleRequestResponse,
      handleRequestStream,
      handleRequestChannel
    }
  where
    handleFireNForgetRequest conn rq = handleRouteRequest (\k -> handleRoute k conn rq) conn rq
    handleRequestResponse conn rq = handleRouteRequest (\k -> handleRoute k conn rq) conn rq
    handleRequestStream conn rq = handleRouteRequest (\k -> handleRoute k conn rq) conn rq
    handleRequestChannel conn rq = handleRouteRequest (\k -> handleRoute k conn rq) conn rq
    handleRouteRequest ::
      forall rq m a.
      ( HasField "metadataPayload" rq RSocket.DataPayload,
        HasField "streamId" rq RSocket.StreamId,
        MonadIO m,
        LiftAppM m
      ) =>
      (RouteKey -> RSocket.MimeType -> RSocket.Metadata -> m a) ->
      ErasedConnection (AppM IO IO) ->
      rq ->
      m a
    handleRouteRequest f conn rq = do
      metadata <- liftAppM $ (RSocket.parseMetadata conn.logger metadataMimeType rq.metadataPayload :: AppM IO IO RSocket.Metadata)
      let !streamId = rq.streamId.unStreamId
      let !remoteAddress = T.pack $! show conn.handle.addr
      $(logInfoVIO ['remoteAddress, 'streamId]) ("Parsed metadata: " <> T.pack (show metadata))
      let normalisedDataMime = normaliseMimeType $ fromMaybe dataMimeType $ dataMime metadata
      case route metadata >>= \rs -> (mapMaybe parseRoute rs) ^? _head of
        Just routeKey -> f routeKey normalisedDataMime metadata
        Nothing -> liftIO $ throwIO NoRouteMetadata
    route (RoutingMetadata routeTags) = Just routeTags
    route (CompositeMetadata ms) = mapMaybe route ms ^? _head
    route _ = Nothing
    dataMime (DataMimeType mime) = Just mime
    dataMime (CompositeMetadata ms) = mapMaybe dataMime ms ^? _head
    dataMime _ = Nothing

class LiftAppM m where
  liftAppM :: AppM IO IO a -> m a

instance LiftAppM (AppM IO IO) where
  liftAppM = Prelude.id

instance LiftAppM (ContT () (AppM IO IO)) where
  liftAppM = lift
