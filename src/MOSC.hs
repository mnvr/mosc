module MOSC where

import Control.Concurrent
import Data.Maybe
import Data.Time
import Network.Socket
import Sound.Osc.Fd

printVersion :: IO ()
printVersion = do
  forkIO requestVersion
  receiveVersion >>= putStrLn

receiveVersion :: IO String
receiveVersion = do
  u <- udpListener
  m <- recvMessage u
  return (toString m)

toString :: Maybe Message -> String
toString (Just ms) = show ms
toString Nothing = "ERR"

requestVersion :: IO ()
-- requestVersion = udpListener >>= (\u -> sendTo u (Packet_Message
-- versionMessage) Network.Socket.addr)
requestVersion = putStrLn "test"

-- | Perform DNS resolution to convert a host:port combination into an AddrInfo
resolve :: String -> Int -> IO (Maybe AddrInfo)
resolve host port =
  listToMaybe <$> getAddrInfo (Just defaultHints) (Just host) (Just (show port))

versionMessage :: Message
versionMessage = Message "/version" [string ""]

udpOut :: IO Udp
udpOut = openUdp "127.0.0.1" 57110

udpIn :: IO Udp
udpIn = openUdp "127.0.0.1" 44440 -- 0 allocates a random port?

-- | A UDP socket
--
-- We can both send and receive using the same socket. When communicating with
-- SuperCollider, it is imperative that we send on the same socket that we're
-- listening on since SuperCollider sends replies there.
udpListener :: IO Udp
udpListener = udpServer "127.0.0.1" 44440
