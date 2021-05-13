module RepaMatMul where

import Data.Array.Repa (Array(..), U(..), Z(..), DIM2(..), (:.)(..))
import Data.Array.Repa as Repa

matMul
  :: (Monad m, Num a)
  => Array U DIM2 a
  -> Array U DIM2 a
  -> m (Array U DIM2 a)
matMul a b = sumP (Repa.zipWith (*) aRepl bRepl)
    where
      t     = Repa.transpose b
      aRepl = extend (Z :.All :.colsB :.All) a
      bRepl = extend (Z :.rowsA :.All :.All) t
      (Z :.colsA :.rowsA) = extent a
      (Z :.colsB :.rowsB) = extent b
