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
      (Z :. aCols :. aRows) = Repa.extent a
      (Z :. bCols :. bRows) = Repa.extent b
      aExtension = Repa.extend (Z :. All :. bCols :. All) a
      bExtension = Repa.extend (Z :. aRows :. All :. All) bTranspose
