module MonadParTree where

import Tree.SerialTree (Tree(..))

-- Test whether all elements match the given predicate (in parallel)
checkAllPar :: (a -> Bool) -> Tree a -> Par Bool
checkAllPar pred (Leaf value) = return $ pred value
checkAllPar pred (Node leftTree rightTree) = do
  al' <- spawn $ checkAllPar pred leftTree
  ar  <- checkAllPar pred rightTree
  al  <- get al'
  return (al && ar)
