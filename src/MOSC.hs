module MOSC where

import Control.Concurrent
import Control.Exception
import Data.Data
import Data.Maybe
import Data.Time
import Network.Socket
import Sound.Osc.Fd

data MOSCException = HostNameResolutionFailed {host :: String, port :: Int}
  deriving (Show, Typeable)

instance Exception MOSCException

printVersion :: IO ()
printVersion = do
  addr <- resolveTarget
  sock <- targetListener
  forkIO $ requestVersion sock addr
  receiveVersion sock >>= putStrLn

receiveVersion :: Udp -> IO String
receiveVersion sock = do
  m <- recvMessage sock
  return (toString m)

toString :: Maybe Message -> String
toString (Just ms) = show ms
toString Nothing = "ERR"

requestVersion :: Udp -> AddrInfo -> IO ()
requestVersion sock addr = sendTo sock (Packet_Message versionMessage) (addrAddress addr)

-- | Return an 'AddrInfo' representing 'targetHost':'targetPort'
resolveTarget :: IO AddrInfo
resolveTarget = resolve targetHost targetPort >>= toE
  where
    toE = maybe (throwIO ex) pure
    ex = (HostNameResolutionFailed {host = targetHost, port = targetPort})

-- | Perform DNS resolution to convert a host:port combination into an
-- 'AddrInfo'
resolve :: String -> Int -> IO (Maybe AddrInfo)
resolve host port =
  listToMaybe <$> getAddrInfo (Just defaultHints) (Just host) (Just (show port))

versionMessage :: Message
versionMessage = Message "/version" [string ""]

udpOut :: IO Udp
udpOut = openUdp "127.0.0.1" 57110

udpIn :: IO Udp
udpIn = openUdp "127.0.0.1" 44440 -- 0 allocates a random port?

-- | A UDP socket listening on targetHost:targetPort
--
-- We can both send and receive using the same UDP socket. When communicating
-- with SuperCollider, it is imperative that we send on the same socket that
-- we're listening on since SuperCollider sends replies there.
targetListener :: IO Udp
targetListener = udpServer targetHost targetPort

-- | The hostname where we should send OSC messages
targetHost :: String
targetHost = "127.0.0.1"

-- | The port on the 'targetHost' where we should send OSC messages
targetPort :: Int
targetPort = 57110
