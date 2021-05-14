module Main where

import System.Environment
import Fib.MonadParFib
import Clock
import Control.Monad.Par

main :: IO ()
main = do
  [nth] <- getArgs
  startTime <- currentTime
  let answer = runPar $ nfib $ read nth
  print answer
  timeElapsedSince startTime
