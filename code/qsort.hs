module Main where

import Control.Monad.ST
import Data.Array.ST
import Data.Foldable
import Control.Monad

swap :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
swap arr i j = do
  tmp <- readArray arr i
  tmp2 <- readArray arr j
  writeArray arr i tmp2
  writeArray arr j tmp

foreachWith :: (Monad m) => [i] -> a -> (i -> a -> m a) -> m a
foreachWith is acc f = foreachWith' is (return $ acc) f
    where foreachWith' [] acc f = acc
          foreachWith' (i:is) acc f = foreachWith' is (acc >>= (f i)) f

partition :: (MArray a e m, Ix i, Enum i, Num i, Ord e) => a i e -> i -> i -> m i
partition arr left right = do
    pivotValue <- readArray arr right
    storeIndex <- foreachWith [left..right-1] left (\i storeIndex -> do
        val <- readArray arr i
        if val <= pivotValue
          then do
                 swap arr i storeIndex
                 return (storeIndex + 1)
          else do
                 return storeIndex )
    swap arr storeIndex right
    return storeIndex

qsort arr left right = when (right > left) $ do
    pivot <- partition arr left right

    qsort arr left (pivot - 1)
    qsort arr (pivot + 1) right

sortList xs = runST $ do
    let lastIndex = length xs - 1
    arr <- newListArray (0,lastIndex) xs :: ST s (STUArray s Int Int)
    qsort arr 0 lastIndex
    newXs <- getElems arr
    return newXs

main = do
  putStrLn $ show $ sortList [5, 4, 2, 1, 8, 7, 3, 6]
