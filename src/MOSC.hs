module MOSC where

import Control.Concurrent
import Control.Exception
import Data.Data
import Data.Maybe
import Data.Time
import qualified Network.Socket as N
import qualified Sound.Osc.Fd as O

data MOSCException = HostNameResolutionFailed {host :: String, port :: Int}
  deriving (Show, Typeable)

instance Exception MOSCException

printVersion :: IO ()
printVersion = do
  addr <- resolveTarget
  sock <- udpSocket
  forkIO $ requestVersion sock addr
  receiveVersion sock >>= putStrLn

receiveVersion :: O.Udp -> IO String
receiveVersion sock = do
  m <- O.recvMessage sock
  return (toString m)

toString :: Maybe O.Message -> String
toString (Just ms) = show ms
toString Nothing = "ERR"

requestVersion :: O.Udp -> N.AddrInfo -> IO ()
requestVersion sock addr =
  O.sendTo sock (O.Packet_Message versionMessage) (N.addrAddress addr)

-- | Return an 'AddrInfo' representing 'targetHost':'targetPort'
resolveTarget :: IO N.AddrInfo
resolveTarget = resolve targetHost targetPort >>= toE
  where
    toE = maybe (throwIO ex) pure
    ex = (HostNameResolutionFailed {host = targetHost, port = targetPort})

-- | Perform DNS resolution to convert a host:port combination into an
-- 'AddrInfo'
resolve :: String -> Int -> IO (Maybe N.AddrInfo)
resolve host port =
  listToMaybe
    <$> N.getAddrInfo (Just N.defaultHints) (Just host) (Just (show port))

versionMessage :: O.Message
versionMessage = O.Message "/version" [O.string ""]

-- | A UDP socket listening on localhost, at an arbitrary port
--
-- We can both send and receive using the same UDP socket. When communicating
-- with SuperCollider, it is imperative that we send on the same socket that
-- we're listening on since SuperCollider sends replies there.
udpSocket :: IO O.Udp
udpSocket = O.udpServer "127.0.0.1" 44440 -- 0 allocates a random port?

-- | The hostname where we should send OSC messages
targetHost :: String
targetHost = "127.0.0.1"

-- | The port on the 'targetHost' where we should send OSC messages
targetPort :: Int
targetPort = 57110
