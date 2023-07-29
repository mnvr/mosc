{-# LANGUAGE NumericUnderscores #-}

module MOSC where

import Control.Concurrent
import Data.Time
import Sound.Osc
import qualified Sound.Osc as Sound.Osc.Fd

version :: IO String
version = show <$> Data.Time.getCurrentTime

foreverNow :: Int -> IO ()
foreverNow t = do
  version >>= putStrLn
  -- putStrLn versionMessage
  threadDelay (t * 1_000_000)
  foreverNow t

versionMessage :: Message
versionMessage = message "/version" []

-- transport = withTransport u1 (\fd -> Sound.Osc.Fd.sendMessage fd (Sound.Osc.Fd.Packet_Message "/n" []))
--   where
--     u1 = openUdp "127.0.0.1" 57300
