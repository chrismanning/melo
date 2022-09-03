{-# LANGUAGE UndecidableInstances #-}

module Melo.Network.Discovery where

import Control.Concurrent
import Control.Exception.Safe hiding (throwTo)
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (find)
import Data.Functor
import Data.IP
import Data.Maybe
import Data.String
import Data.Text (Text)
import Data.Text.Encoding
import Data.Unique
import Data.Word (Word16)
import Melo.Common.Logging
import Network.DNS hiding (TYPE (..))
import Network.DNS qualified as DNS
import Network.Multicast
import Network.Socket
import Network.Socket.ByteString
import System.Random hiding (split)

class Monad m => NetworkDiscovery m where
  discoverService :: DiscoverServiceName -> m [Service]

newtype DiscoverServiceName = DiscoverServiceName Text
  deriving (Show, Eq)
  deriving newtype (IsString)

data Service = Service
  { name :: ServiceName,
    addr :: SockAddr
  }
  deriving (Show, Eq)

instance
  {-# OVERLAPPABLE #-}
  ( Monad (t m),
    MonadTrans t,
    NetworkDiscovery m
  ) =>
  NetworkDiscovery (t m)
  where
  discoverService = lift . discoverService

newtype NetworkDiscoveryIOT m a = NetworkDiscoveryIOT
  { runNetworkDiscoveryIOT :: m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadBase b,
      MonadBaseControl b,
      MonadCatch,
      MonadThrow,
      MonadMask
    )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance (MonadIO m) => NetworkDiscovery (NetworkDiscoveryIOT m) where
  discoverService (DiscoverServiceName svc) = liftIO $ do
    identifier <- randomIO
    let query =
          encodeMulticastDNSQuestion
            identifier
            MulticastDNSQuestion
              { name = svc,
                queryType = PTR,
                unicastResponse = UnicastResponse
              }
    $(logDebugShowIO) query
    withSocketsDo $
      bracket mdnsSender (close . fst) $ \(sock, sendAddr) -> do
        setLoopbackMode sock noLoopback
        sendAllTo sock query sendAddr
        r <- repeatFor (seconds 3) $
          handleIO logAndContinue $ do
            buf <- recv sock (fromIntegral maxUdpSize)
            case decode buf of
              Left e -> throwIO e
              Right msg@(DNSMessage {header = DNSHeader {identifier = responseId}, ..}) | responseId == identifier -> do
                $(logDebugShowIO) msg
                case find (\ResourceRecord {rrtype} -> rrtype == DNS.SRV) additional of
                  Just ResourceRecord {rdata = (RD_SRV _ _ port _), rrname = srvname} ->
                    pure $
                      catMaybes $
                        additional <&> \ResourceRecord {rdata} ->
                          Service (C8.unpack srvname) <$> extractAddress rdata (fromIntegral port)
                  _ -> pure []
              _ -> pure []
        pure $ concat r
    where
      extractAddress (RD_A address) port = Just $ SockAddrInet port (toHostAddress address)
      extractAddress (RD_AAAA address) port = Just $ SockAddrInet6 port 0 (toHostAddress6 address) 0
      extractAddress _ _ = Nothing
      logAndContinue e = do
        $(logErrorIO) $ "mDNS error: " <> show e
        pure mempty

newtype Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
  show _ = "<<timeout>>"

instance Exception Timeout

repeatFor :: Int -> IO a -> IO [a]
repeatFor n f
  | n <= 0 = return []
  | otherwise = do
      pid <- myThreadId
      ex <- fmap Timeout newUnique
      results <- newMVar []
      handleJust
        (\e -> if e == ex then Just () else Nothing)
        (\_ -> pure ())
        ( bracket
            (forkIOWithUnmask (\unmask -> unmask (threadDelay n >> throwTo pid ex)))
            (killThread)
            ( \_ -> forever $ do
                r <- f
                modifyMVar_ results (pure . (r :))
            )
        )
      readMVar results

seconds :: Int -> Int
seconds = (* (10 ^ (6 :: Int)))

mdnsMulticastHost :: HostName
mdnsMulticastHost = "224.0.0.251"

mdnsMulticastPort :: PortNumber
mdnsMulticastPort = 5353

mdnsSender :: IO (Socket, SockAddr)
mdnsSender = multicastSender mdnsMulticastHost mdnsMulticastPort

mdnsReceiver :: IO Socket
mdnsReceiver = multicastReceiver mdnsMulticastHost mdnsMulticastPort

data MulticastDNSQuestion = MulticastDNSQuestion
  { name :: !Text,
    queryType :: !DNSRecordType,
    unicastResponse :: !UnicastResponse
  }
  deriving (Show, Eq)

data DNSRecordType
  = A
  | AAAA
  | PTR
  | SRV
  | TXT
  deriving (Show, Eq)

data UnicastResponse = UnicastResponse | MulticastResponse
  deriving (Show, Eq)

encodeMulticastDNSQuestion :: Word16 -> MulticastDNSQuestion -> ByteString
encodeMulticastDNSQuestion identifier MulticastDNSQuestion {..} =
  toStrict $
    toLazyByteString $
      word16BE identifier
        <> word16BE 0
        <> word16BE 1
        <> word16BE 0
        <> word16BE 0
        <> word16BE 0
        <> encodeDomain name
        <> word8 0
        <> encodeDNSRecordType queryType
        <> encodeClass unicastResponse

encodeDomain :: Text -> Builder
encodeDomain domain =
  foldMap (\part -> int8 (fromIntegral $ BS.length part) <> byteString part) $
    C8.split '.' (encodeUtf8 domain)

encodeDNSRecordType :: DNSRecordType -> Builder
encodeDNSRecordType A = word16BE 0x01
encodeDNSRecordType AAAA = word16BE 0x1C
encodeDNSRecordType PTR = word16BE 0x0C
encodeDNSRecordType SRV = word16BE 0x21
encodeDNSRecordType TXT = word16BE 0x10

encodeClass :: UnicastResponse -> Builder
encodeClass UnicastResponse = word16BE 0x8001
encodeClass MulticastResponse = word16BE 1
