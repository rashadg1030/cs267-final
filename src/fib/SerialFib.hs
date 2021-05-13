module SerialFib where

nfib
  :: Int -- The nth number in the fibonacci sequence that you want
  -> Int -- The result
nfib 0 = 0
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2)
