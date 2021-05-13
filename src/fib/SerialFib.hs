module SerialFib where

nfib :: Int -> Int
nfib 0 = 0
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2)
