module Tree.SerialTree where

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
  deriving (Eq, Show)

-- Check that all values in the tree satisfy a given predicate
checkAll :: (a -> Bool) -> Tree a -> Bool
checkAll pred (Leaf value) = pred value
checkAll pred (Node leftTree rightTree) = (checkAll pred leftTree) && (checkAll pred rightTree)
