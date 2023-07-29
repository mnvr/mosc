module Main where

import Data.Maybe
import MOSC
import System.Environment
import Text.Read

main :: IO ()
main = do
  d <- parseDuration <$> getArgs
  printMessage d
  foreverNow d

parseDuration :: [String] -> Int
parseDuration args = fromMaybe 3 $ listToMaybe args >>= readMaybe

printMessage :: Int -> IO ()
printMessage d = putStrLn ("Every " ++ show d ++ " seconds I'll remind you, that I exist. I am time.")
