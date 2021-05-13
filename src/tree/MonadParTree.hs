module MonadParTree where

import SerialTree (Tree(..))
import Control.Monad.Par

-- Test whether all elements match the given predicate (in parallel)
checkAllPar :: (a -> Bool) -> Tree a -> Par Bool
checkAllPar pred (Leaf value) = return $ pred value
checkAllPar pred (Node leftTree rightTree) = do
  al' <- spawn $ checkAllPar pred leftTree
  ar  <- checkAllPar pred rightTree
  al  <- get al'
  return (al && ar)
