module Melo.Common.API where

import Control.Concurrent.Class.MonadSTM.Strict.TQueue
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Data.Aeson qualified as JSON
import Data.ByteString.Builder
import Data.ByteString.Lazy qualified as L
import Data.Hashable
import Data.Text qualified as T
import Melo.Common.Logging
import Melo.Common.Monad hiding (atomically)
import Network.RSocket qualified as RSocket

newtype RouteKey = RouteKey {unRouteKey :: Text}
  deriving newtype (TextShow, Eq, Hashable)
  deriving (Show) via FromTextShow RouteKey

data ErrorResponse = ErrorResponse
  { errorCode :: ErrorCode,
    message :: Text
  }
  deriving (Generic)
  deriving (TextShow) via FromGeneric ErrorResponse
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions ErrorResponse

data ErrorCode
  = RouteMetadataMissing
  | RouteNotFound
  | ServiceError
  deriving (Generic)
  deriving (TextShow) via FromGeneric ErrorCode
  deriving (FromJSON, ToJSON) via CustomJSON JSONOptions ErrorCode

data ApiException
  = NoRouteMetadata
  | UnknownRoute RouteKey
  | ServiceException Text
  deriving (Show, Exception)

apiStreamExceptionHandlers :: RSocket.ErasedConnection (AppM IO IO) -> [Handler (AppM IO IO) ()]
apiStreamExceptionHandlers conn =
  [ Handler \(RSocket.StreamException (RSocket.StreamId streamId) e) -> do
      let !cause = displayException e
      let !remoteAddress = T.pack $! show conn.handle.addr
      $(logErrorVIO ['cause, 'remoteAddress, 'streamId]) "Error in RSocket connection"
      RSocket.writeFrame
        conn
        ( RSocket.ErrorFrame $
            RSocket.Error (RSocket.StreamId streamId) (RSocket.errorCode RSocket.ApplicationError) (RSocket.DataPayload (JSON.encode (toErrorResponse e)))
        ),
    Handler \case
      e@(RSocket.ConnectionException code) -> do
        let !cause = displayException e
        let !remoteAddress = T.pack $! show conn.handle.addr
        $(logErrorVIO ['cause, 'remoteAddress]) "Unrecoverable error in RSocket connection"
        RSocket.writeFrame conn (RSocket.ErrorFrame $ RSocket.Error (RSocket.StreamId 0) (RSocket.errorCode code) mempty)
        $(logWarnVIO ['remoteAddress]) "Stopping connection"
        -- Add `ShutdownItem` to read queue to effectively stop the connection.
        atomically do
          writeTQueue conn.readQueue RSocket.ShutdownItem
      e@(RSocket.NoSuchStreamException (RSocket.StreamId streamId)) -> do
        let !cause = displayException e
        let !remoteAddress = T.pack $! show conn.handle.addr
        $(logErrorVIO ['cause, 'remoteAddress, 'streamId]) "Request received for unknown RSocket stream"
  ]
  where
    toErrorResponse :: SomeException -> ErrorResponse
    toErrorResponse e = case fromException e of
      Just NoRouteMetadata ->
        ErrorResponse
          { errorCode = RouteMetadataMissing,
            message = "route metadata is required"
          }
      Just (UnknownRoute route) ->
        ErrorResponse
          { errorCode = RouteNotFound,
            message = showt route
          }
      Just (ServiceException message) ->
        ErrorResponse
          { errorCode = ServiceError,
            message
          }
      Nothing ->
        ErrorResponse
          { errorCode = ServiceError,
            message = T.pack (displayException e)
          }

data PayloadWrapper
  = RawPayload L.ByteString
  | TypedPayload RSocket.MimeType L.ByteString
  | forall a. (ToJSON a) => JsonPayload a

buildStreamPayload :: PayloadWrapper -> RSocket.Metadata -> RSocket.StreamId -> RSocket.Payload
buildStreamPayload r metadata streamId = buildPayload r metadata streamId False

buildPayload :: PayloadWrapper -> RSocket.Metadata -> RSocket.StreamId -> Bool -> RSocket.Payload
buildPayload r metadata' streamId complete =
  RSocket.Payload
    { streamId,
      complete,
      next = True,
      metadataPayload = RSocket.DataPayload (toLazyByteString $ RSocket.renderMetadata metadata),
      payload
    }
  where
    payload = case r of
      JsonPayload a -> RSocket.DataPayload (JSON.encode a)
      RawPayload bs -> RSocket.DataPayload bs
      TypedPayload _mime bs -> RSocket.DataPayload bs
    metadata = case r of
      JsonPayload _ -> prepareMetadata metadata' (Just (RSocket.MimeTypeId RSocket.ApplicationJson))
      TypedPayload mime _ -> prepareMetadata metadata' (Just mime)
      _ -> prepareMetadata metadata' Nothing
    prepareMetadata (RSocket.CompositeMetadata ms) (Just mime) = RSocket.CompositeMetadata (RSocket.DataMimeType mime : filter (isn't _DataMimeType) ms)
    prepareMetadata (RSocket.CompositeMetadata ms) Nothing = RSocket.CompositeMetadata ms
    prepareMetadata (RSocket.DataMimeType _) (Just mime) = RSocket.CompositeMetadata [RSocket.DataMimeType mime]
    prepareMetadata metadata (Just mime) = RSocket.CompositeMetadata (metadata : [RSocket.DataMimeType mime])
    prepareMetadata metadata Nothing = RSocket.CompositeMetadata [metadata]
