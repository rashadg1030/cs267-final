module Lib
    ( serialMain
    , parallelMain
    ) where

import qualified Serial (main)
import qualified Parallel (main)

serialMain :: IO ()
serialMain = Serial.main

parallelMain :: IO ()
parallelMain = Parallel.main
