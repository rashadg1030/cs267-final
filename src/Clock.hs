module Clock where

import Data.Time.Clock
import Text.Printf

currentTime = getCurrentTime

timeElapsedSince t0 = do
  t1 <- getCurrentTime
  printf "time: %.2fs\n" (realToFrac (diffUTCTime t1 t0) :: Double)
