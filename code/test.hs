import Data.Array
import Data.Ix
import System.Random
import Data.List
import System.CPUTime

data PositionComponent = PositionComponent { x :: Double , y :: Double , z ::Double }

chunks :: Int -> [a] -> [[a]]
chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

updatePos :: (Enum i, Ix i) => Array i PositionComponent -> Array i PositionComponent
updatePos a = array (s, e) [ (i, update (a ! i)) | i <- [s..e] ]
    where (s, e) = bounds a
          update pc = pc { y = (y pc) * 200.0 }

toPC [x, y, z] = PositionComponent { x = x, y = y, z = z }

getCPUTimeDouble :: IO Double
getCPUTimeDouble = do t <- System.CPUTime.getCPUTime; return ((fromInteger t) * 1e-12)

main :: IO ()
main = do
    gen <- getStdGen
    let rands = (randomRs (0.0, 100.0) gen)
    let ichunks = take 100000000 $ chunks 3 rands
    let entities = listArray (0,100000000 - 1) $ map toPC ichunks
    start <- getCPUTimeDouble
    let nes = updatePos entities
    stop <- getCPUTimeDouble
    putStrLn $ "Execution time: " ++ show (stop - start)
