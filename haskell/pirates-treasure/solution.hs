import Control.Monad ( replicateM )
import Data.Maybe ( catMaybes )

data Coordinate = Coordinate {
      getRow :: Int
    , getCol :: Int
} deriving (Eq)

instance Show Coordinate where
    show coordinate = "Coordinate{" ++ (show . getRow) coordinate ++ "," ++ (show . getCol) coordinate ++ "}"

type Index = Int
type Value = Int
type Matrix = [[Int]]

showMatrix :: Matrix -> String
showMatrix = unlines . fmap show

lineToArray :: String -> [Int]
lineToArray = fmap (read :: String -> Int) . words

readMatrix :: Int -> IO Matrix
readMatrix n = replicateM n (lineToArray <$> getLine)

getMatrix :: IO Matrix
getMatrix = do
    _ <- getLine
    n  <- fmap read getLine :: IO Int
    readMatrix n

getValue :: Coordinate -> Matrix -> Maybe Value
getValue coordinate matrix =
    if 0 <= row && row < height && 0 <= col && col < width
    then Just $ matrix !! row !! col
    else Nothing
    where
        row = getRow coordinate
        col = getCol coordinate
        height = length matrix
        width = (length . head) matrix

getNeighbors :: Coordinate -> [Coordinate]
getNeighbors center = [
    Coordinate r c |
        r <- [row-1 .. row+1],
        c <- [col-1 .. col+1],
        (r /= row) || (c /= col)
    ]
    where row = getRow center
          col = getCol center

isTreasure :: Coordinate -> Matrix -> Bool
isTreasure coordinate matrix =
    getValue coordinate matrix == Just 0
    &&
    neightborsAreAllObstacles
    where neightbors = getNeighbors coordinate
          values = fmap (`getValue` matrix) neightbors
          neightborsAreAllObstacles = all (== 1) (catMaybes values)

getAllCoordinates :: Matrix -> [Coordinate]
getAllCoordinates matrix = [
    Coordinate r c |
        r <- [0..(height-1)],
        c <- [0..(width-1)]
    ] where height = length matrix
            width = (length . head) matrix

findTreasure :: Matrix -> Coordinate
findTreasure matrix = let
    allCoordinates = getAllCoordinates matrix
    coordinateIsTreasure = fmap (\coord -> (coord, isTreasure coord matrix)) allCoordinates
    coordinatesWithTreasures = filter snd coordinateIsTreasure
    in (fst . head) coordinatesWithTreasures

printCoordinate :: Coordinate -> IO()
printCoordinate coordinate = putStrLn $ show col ++ " " ++ show row
    where row = getRow coordinate
          col = getCol coordinate

main :: IO ()
main = getMatrix >>= (printCoordinate . findTreasure)
-- main = getMatrix >>= print . getValue (Coordinate 3 2)
-- main = (print . getNeighbors) (Coordinate 3 2)
-- main = getMatrix >>= print . isTreasure (Coordinate 3 2)
-- main = getMatrix >>= putStrLn . showMatrix
