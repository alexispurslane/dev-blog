import Data.Ix
import System.Random
import Data.List
import System.CPUTime
import Control.Monad.ST
import Data.Array.ST
import Data.STRef
import Data.Foldable
import Debug.Trace

data PositionComponent s = PositionComponent { x :: STRef s Double , y :: STRef s Double , z :: STRef s Double }

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

updatePos :: (Num i, Ix i) => i -> STArray s i (PositionComponent s) -> ST s (STArray s i (PositionComponent s))
updatePos 0 a = return a
updatePos i a = do
    thing <- readArray a i
    modifySTRef (y thing) (*200.0)
    updatePos (i - 1) a

toPC :: [Double] -> ST s (PositionComponent s)
toPC [x, y, z] = do
    x <- newSTRef x
    y <- newSTRef y
    z <- newSTRef z
    return $ PositionComponent { x = x, y = y, z = z }

getCPUTimeDouble :: IO Double
getCPUTimeDouble = do t <- System.CPUTime.getCPUTime; return ((fromInteger t) * 1e-12)

num = 100000000

main :: IO ()
main = do
    gen <- getStdGen
    let rands = (randomRs (0.0, 100.0) gen)
    let ichunks = take num $ chunks 3 rands :: [[Double]]
    start <- getCPUTimeDouble
    let ne = runST $ do
         e0 <- sequence $ map toPC ichunks
         e1 <- newListArray (0,num) e0
         updatePos (num-1) e1
         return ()
    stop <- getCPUTimeDouble
    putStrLn $ "Execution time: " ++ show (stop - start)
