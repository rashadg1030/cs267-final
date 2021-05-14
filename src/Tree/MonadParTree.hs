module Tree.MonadParTree where

import Tree.SerialTree (Tree(..))
import Control.Monad.Par

-- Test whether all elements match the given predicate (in parallel)
checkAllPar :: (a -> Bool) -> Tree a -> Par Bool
checkAllPar pred (Leaf value) = return $ pred value
checkAllPar pred (Node leftTree rightTree) = do
  leftTreeComputation <- spawn $ checkAllPar pred leftTree
  rightTreeResult  <- checkAllPar pred rightTree
  leftTreeResult  <- get leftTreeComputation
  return $ leftTreeResult && rightTreeResult
