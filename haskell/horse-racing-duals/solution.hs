import System.IO
import Control.Monad
import Data.List (sort)

readInput :: IO Int
readInput = (read :: String -> Int) <$> getLine

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    n <- readInput
    l <- replicateM n readInput
    let sortedList = sort l
    let diff = [s - f | (f, s) <- zip (init sortedList) (tail sortedList)]
    let lowest = minimum diff
    print lowest
    return ()
