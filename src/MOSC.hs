module MOSC where

import Control.Concurrent
import Data.Maybe
import Data.Time
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
requestVersion = udpOut >>= (`sendMessage` versionMessage)

versionMessage :: Message
versionMessage = Message "/version" []

udpOut :: IO Udp
udpOut = openUdp "127.0.0.1" 57110

udpIn :: IO Udp
udpIn = openUdp "127.0.0.1" 44440 -- 0 allocates a random port?

udpListener = udpServer "127.0.0.1" 44440
