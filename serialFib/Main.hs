module Main where

import System.Environment
import Fib.SerialFib
import Clock

main :: IO ()
main = do
  [nth] <- getArgs
  startTime <- currentTime
  print $ nfib $ read nth
  timeElapsedSince startTime
