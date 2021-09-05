import System.IO
import Control.Monad

data Cell = Alive | Dead deriving (Eq, Show)

getHWN = map (read :: String -> Int) . words <$> getLine
getRawLivenessCondition = getLine
getRawInitialCondition = flip replicateM getLine

charToCell :: Char -> Cell
charToCell char = if char == '.' then Alive else Dead

charsToCells :: [Char] -> [Cell]
charsToCells = map charToCell

rawInitialConditionToCells :: [[Char]] -> [[Cell]]
rawInitialConditionToCells = map charsToCells

skipLine :: IO ()
skipLine = putStrLn ""

displayCells :: [[Cell]] -> IO ()
displayCells cells = do
    skipLine
    putStrLn "Game status:"
    putStrLn . unlines . map (show . show) $ cells
    skipLine

makeDeadLine :: [[Cell]] -> [Cell]
makeDeadLine cells = replicate (length (head cells)) Dead

addDeadCells :: [[Cell]] -> [[Cell]]
addDeadCells = map (\cells -> Dead : cells ++ [Dead])

addDeadCellsOnTopAndBottom :: [[Cell]] -> [[Cell]]
addDeadCellsOnTopAndBottom init = makeDeadLine init : init ++ [makeDeadLine init]

addAllDeadCells :: [[Cell]] -> [[Cell]]
addAllDeadCells = addDeadCellsOnTopAndBottom . addDeadCells

getSurroundingCells :: (Int, Int) -> [[Cell]] -> [Cell]
getSurroundingCells (x,y) cells = [
      cells !! (y-1) !! (x-1)
    , cells !! (y-1) !!  x
    , cells !! (y-1) !! (x+1)
    , cells !!  y    !! (x-1)
    , cells !!  y    !! (x+1)
    , cells !! (y+1) !! (x-1)
    , cells !! (y+1) !!  x
    , cells !! (y+1) !! (x+1)
    ]

countAlive :: [Cell] -> Int
countAlive = length . filter (==Alive)

rawLivenessConditionToList :: Int -> [Char] -> [Int] -> [Int]
rawLivenessConditionToList _ [] l  = l
rawLivenessConditionToList n (c:cs) l = rawLivenessConditionToList (n+1) cs (if c == '1' then n : l else l)

toLivenessList :: [Char] -> [Int]
toLivenessList raw = rawLivenessConditionToList 0 raw []

toLivenessFunction :: [Char] -> ([Cell] -> Cell)
toLivenessFunction raw cells = if
    or ((==) <$> toLivenessList raw <*> [length (filter (==Alive) cells)])
    then Alive
    else Dead

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    
    [h, w, n] <- getHWN

    aliveRawCondition <- getRawLivenessCondition
    birthRawCondition <- getRawLivenessCondition
    initialRawCondition <- getRawInitialCondition h

    print (h, w, n)
    print aliveRawCondition
    print $ toLivenessList aliveRawCondition
    print $ toLivenessList birthRawCondition
    print birthRawCondition
    displayCells . addAllDeadCells . rawInitialConditionToCells $ initialRawCondition
    displayCells [getSurroundingCells (1, 1) ((addAllDeadCells . rawInitialConditionToCells) initialRawCondition)]
    print $ countAlive (getSurroundingCells (1, 1) ((addAllDeadCells . rawInitialConditionToCells) initialRawCondition))
    print $ toLivenessFunction "000000000" [Alive, Dead, Dead, Dead, Dead]