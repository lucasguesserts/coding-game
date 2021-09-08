import System.IO
import Control.Monad

readInput :: IO [Int]
readInput = map (read :: String -> Int) . words <$> getLine

addBeginEnd :: Int -> [Int] -> [Int]
addBeginEnd m l = 0 : l ++ [m]

allMeasures :: [Int] -> [Int]
allMeasures ms = [mf - mi | mf <- ms, mi <- ms, mi < mf]

isSquare :: (Int, Int) -> Bool
isSquare = uncurry (==)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    [w, h, countx, county] <- readInput
    ws <- allMeasures . addBeginEnd w <$> readInput
    hs <- allMeasures . addBeginEnd h <$> readInput

    print . length . filter isSquare $ [(w,h) | w <- ws, h <- hs]
    return ()
