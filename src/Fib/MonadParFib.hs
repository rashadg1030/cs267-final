module Fib.MonadParFib where

import Control.Monad.Par

nfib :: Int -> Par Int
nfib n = do
  firstComputation <- spawn (nfib (n - 1))
  secondValue <- nfib (n - 2)
  firstValue <- get firstComputation
  return (firstValue + secondValue)
