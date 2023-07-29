{-# LANGUAGE NumericUnderscores #-}

module MOSC where

import Control.Concurrent
import Data.Time

version :: IO String
version = show <$> Data.Time.getCurrentTime

foreverNow :: Int -> IO ()
foreverNow t = do
  version >>= putStrLn
  threadDelay (t * 1_000_000)
  foreverNow t
