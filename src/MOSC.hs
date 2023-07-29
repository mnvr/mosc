module MOSC where

import Control.Concurrent
import Data.Time
import Sound.Osc.Fd

printVersion :: IO ()
printVersion = udpOut >>= (`sendMessage` versionMessage)

versionMessage :: Message
versionMessage = Message "/version" []

udpOut :: IO Udp
udpOut = openUdp "127.0.0.1" 6012
