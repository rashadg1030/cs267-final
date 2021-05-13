module RepaMatMul where

import Data.Array.Repa (Array(..), U(..), Z(..), DIM2(..), (:.)(..))
import Data.Array.Repa as Repa
import Data.Vector.Unboxed.Base (Unbox(..))

matMul
  :: (Monad m, Num a, Unbox a)
  => Array U DIM2 a
  -> Array U DIM2 a
  -> m (Array U DIM2 a)
matMul a b = Repa.sumP (Repa.zipWith (*) aRepl bRepl)
    where
      bTranspose = Repa.transpose b
      aRepl = Repa.extend (Z :.All :.colsB :.All) a
      bRepl = Repa.extend (Z :.rowsA :.All :.All) bTranspose
      (Z :.colsA :.rowsA) = Repa.extent a
      (Z :.colsB :.rowsB) = Repa.extent b
