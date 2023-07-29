module MOSC where

import Control.Concurrent
import Data.Time
import Sound.Osc.Fd

version :: IO String
version = show <$> Data.Time.getCurrentTime

foreverNow :: Int -> IO ()
foreverNow t = do
  (Sound.Osc.Fd.openUdp "127.0.0.1" 6012) >>= (\r -> Sound.Osc.Fd.sendMessage r $ (Message "/version" []))

-- version >>= putStrLn
-- (Packet.message "/n" [])
-- Sound.Osc.Fd.sendMessage udpOut versionMessage

versionMessage :: Message
versionMessage = Message "/version" []

-- udpOut :: IO Udp
udpOut = openUdp "127.0.0.1" 6012

-- transport = withTransport u1 (\fd -> Sound.Osc.Fd.sendMessage fd (Sound.Osc.Fd.Packet_Message "/n" []))
--   where
--     u1 = openUdp "127.0.0.1" 57300
