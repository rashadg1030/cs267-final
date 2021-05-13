module SerialMatMul where

import Data.Vector (Vector(..), (!))
import qualified Data.Vector as Vector

matMul
  :: (Eq a, Num a)
  => Vector (Vector a)
  -> Vector (Vector a)
  -> Either String (Vector (Vector a))
matMul a b
  | Vector.length a == 0 || Vector.length b == 0 = Left "Empty matrices can't be used"
  | not (isConsistentMatrix a) || not (isConsistentMatrix b) = Left "The dimensions of the matrices are inconsistent"
  | numOfCol a /= numOfRow b = Left "These matrices are incompatible for multiplication"
  | otherwise = Right $ mul a b
  where
    numOfCol m = Vector.length $ Vector.head m
    numOfRow m = Vector.length m

-- Helpers

mul :: Num a => Vector (Vector a) -> Vector (Vector a) -> Vector (Vector a)
mul a b = do
  aRows <- a
  return $ do
    bCols <- transpose b
    return $ Vector.sum $ Vector.zipWith (*) aRows bCols

transpose :: Vector (Vector a) -> Vector (Vector a)
transpose v = Vector.fromList [ Vector.fromList [ v ! col ! row | col <- [0 .. maxCol] ] | row <- [0 .. maxRow] ]
  where
    maxRow = Vector.length v - 1
    maxCol = Vector.length (v ! 0) - 1

isConsistentMatrix :: Eq a => Vector (Vector a) -> Bool
isConsistentMatrix m = allSame $ Vector.map Vector.length m
  where
    allSame :: Eq a => Vector a -> Bool
    allSame v = Vector.and $ Vector.map (== Vector.head v) (Vector.tail v)
