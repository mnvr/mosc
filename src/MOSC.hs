module MOSC where

import Control.Concurrent
import Control.Exception
import Data.Data
import Data.Maybe
import Data.Time
import Network.Socket
import Sound.Osc.Fd

data MOSCException
  = HostNameResolutionFailed {host :: String, port :: Int}
  | NoResponse
  deriving (Show, Typeable)

instance Exception MOSCException

printVersion :: IO ()
printVersion = do
  addr <- resolveTarget
  sock <- newUdpSocket
  forkIO $ requestVersion sock addr
  receiveVersion sock >>= putStrLn

receiveVersion :: Udp -> IO String
receiveVersion sock =
  recvMessage sock >>= maybe (throwIO NoResponse) (pure . show)

requestVersion :: Udp -> AddrInfo -> IO ()
requestVersion sock addr =
  sendTo sock (Packet_Message versionMessage) (addrAddress addr)

-- | Return an 'AddrInfo' representing 'targetHost':'targetPort'
resolveTarget :: IO AddrInfo
resolveTarget = resolve targetHost targetPort >>= toE
  where
    toE = maybe (throwIO ex) pure
    ex = (HostNameResolutionFailed {host = targetHost, port = targetPort})

-- | Perform DNS resolution to convert a host:port combination into an AddrInfo
resolve :: String -> Int -> IO (Maybe AddrInfo)
resolve host port =
  listToMaybe
    <$> getAddrInfo (Just defaultHints) (Just host) (Just (show port))

versionMessage :: Message
versionMessage = Message "/version" [string ""]

-- | A UDP socket listening on localhost, at an arbitrary port
--
-- We can both send and receive using the same UDP socket. When communicating
-- with SuperCollider, it is imperative that we send on the same socket that
-- we're listening on since SuperCollider sends replies there.
newUdpSocket :: IO Udp
newUdpSocket = udpServer "127.0.0.1" 0 -- 0 allocates a random port

-- | The hostname where we should send OSC messages
targetHost :: String
targetHost = "127.0.0.1"

-- | The port on the 'targetHost' where we should send OSC messages
targetPort :: Int
targetPort = 57110
