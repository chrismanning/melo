module Melo.Common.Logging.Loki where

import Control.Exception.Safe
import Control.Monad
import Data.Aeson as A
import Data.Aeson.KeyMap qualified as A
import Data.ByteString.Lazy.Char8 hiding (hPutStrLn)
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LTB
import Data.Time.Clock.System (SystemTime(..), utcToSystemTime)
import Data.Vector qualified as V
import GHC.Generics hiding (from)
import Katip.Core
import Melo.Env
import Network.HTTP.Client as Http
import Network.HTTP.Types as Http
import System.Exit
import System.IO

mkLokiScribe :: LokiConfig -> Http.Manager -> IO (Maybe Scribe)
mkLokiScribe config manager = case config.url of
  Nothing -> pure Nothing
  Just url -> (flip catches) handles do
    readyRequest <- parseRequest (url <> "/ready")
    ready <- httpNoBody readyRequest manager
    unless (statusIsSuccessful ready.responseStatus) do
      fail "could not connect to configured loki instance"

    pure $
      Just
        Scribe
          { liPush,
            scribeFinalizer,
            scribePermitItem = const (pure True)
          }
    where
      liPush :: LogItem a => Item a -> IO ()
      liPush item = do
        let !body = A.encode PushRequest { streams = buildStream item :| []}
        !req <- parseRequest (url <> "/loki/api/v1/push")
          <&> (\r -> r { requestBody = RequestBodyLBS body })
          <&> (\r -> r { requestHeaders = (hContentType, "application/json") : r.requestHeaders })
          <&> (\r -> r { method = methodPost })
        handleAny (\e -> hPutStrLn stderr $ "Failed to push logs to loki: " <> displayException e) do
          response <- httpNoBody req manager
          unless (statusIsSuccessful response.responseStatus) do
            hPutStrLn stderr $ "Failed to push logs to loki: " <> show response.responseStatus
      handles = [Handler handleHttpException, Handler handleAnyE]
      handleHttpException :: HttpException -> IO (Maybe Scribe)
      handleHttpException (HttpExceptionRequest _ e) = die $ show e
      handleHttpException e = die $ displayException e
      handleAnyE :: SomeException -> IO (Maybe Scribe)
      handleAnyE = die . displayException
      buildStream item = Stream {
        stream = buildLabels item,
        values = buildEntry item :| []
      }
      scribeFinalizer = do
        System.IO.putStrLn "loki scribe closing"
        pure ()
      buildEntry :: LogItem a => Item a -> Entry
      buildEntry item = Entry {
        timestamp = from $ utcToSystemTime item._itemTime,
        line = decodeUtf8 $ toStrict $ encode $ A.Object $ buildLine item
      }
      buildLine item = A.fromList [
        ("message", A.String $ buildMessage item._itemMessage),
        ("pid", A.String $ processIDToText item._itemProcess),
        ("level", A.String $ renderSeverity item._itemSeverity),
        ("namespace", A.String (T.intercalate "." (item._itemNamespace.unNamespace ^. _tail))),
        ("tid", A.String (getThreadIdText $ item._itemThread))
        ] <> toObject item._itemPayload
      buildMessage (LogStr b) = LT.toStrict $ LTB.toLazyText b
      buildLabels :: Item a -> A.Value
      buildLabels item = object [
        ("app", A.String (item._itemApp.unNamespace !! 0)),
        ("host", A.String (T.pack item._itemHost)),
        ("env", A.String item._itemEnv.getEnvironment)
        ]

type LokiHttpConnection = Http.Manager

data PushRequest = PushRequest
  { streams :: NonEmpty Stream
  }
  deriving (Generic)

instance ToJSON PushRequest where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data Stream = Stream
  { stream :: A.Value,
    values :: NonEmpty Entry
  }
  deriving (Generic)

instance ToJSON Stream where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

data Entry = Entry
  { timestamp :: LokiTimestamp,
    line :: Text
  }
  deriving (Generic)

instance ToJSON Entry where
  toJSON e = A.Array (V.fromList [A.String e.timestamp.u, A.String e.line])

newtype LokiTimestamp = LokiTimestamp { u :: Text }
  deriving (Generic)
  deriving newtype (ToJSON)

instance From SystemTime LokiTimestamp where
  from t = LokiTimestamp $ showt (t.systemSeconds * 1_000_000_000 + fromIntegral t.systemNanoseconds)

data PushResponse = PushResponse {}
  deriving (Generic)
