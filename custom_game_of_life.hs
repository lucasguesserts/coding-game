import System.IO
import Control.Monad

data Cell = Alive | Dead deriving (Eq, Show)

-- input

getHWN = map (read :: String -> Int) . words <$> getLine
getRawLivenessCondition = getLine
getRawInitialCondition = flip replicateM getLine

charToCell :: Char -> Cell
charToCell char = if char == 'O' then Alive else Dead

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

-- output

cellToChar :: Cell -> Char
cellToChar cell = if cell == Alive then 'O' else '.'

cellsToChars :: [Cell] -> [Char]
cellsToChars = map cellToChar

cellsToOutput :: [[Cell]] -> [[Char]]
cellsToOutput = map cellsToChars

printOutput :: [[Cell]] -> IO ()
printOutput cells = putStrLn . unlines $ cellsToOutput cells

displayOutput :: [[Cell]] -> IO ()
displayOutput cells = do
  skipLine
  putStrLn "Game status:"
  printOutput cells
  skipLine


-- surrounding cells

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

getMainCell :: (Int, Int) -> [[Cell]] -> Cell
getMainCell (x,y) cells = cells !! y !! x

-- make coordinates

makeMyList :: Int -> Int -> Int -> [(Int, Int)]
makeMyList y i limit
  |  i == limit = []
  |  otherwise = (i,y) : makeMyList y (i+1) limit

makeMyLargerList :: Int -> Int -> Int -> [[(Int, Int)]]
makeMyLargerList i h w
  | i == h+1 = []
  | otherwise = makeMyList i 1 (w+1) : makeMyLargerList (i+1) h w

makeCoordinates :: Int -> Int -> [[(Int, Int)]]
makeCoordinates = makeMyLargerList 1

-- liveness function

rawLivenessConditionToList :: Int -> [Char] -> [Int] -> [Int]
rawLivenessConditionToList _ [] l  = l
rawLivenessConditionToList n (c:cs) l = rawLivenessConditionToList (n+1) cs (if c == '1' then n : l else l)

toLivenessList :: [Char] -> [Int]
toLivenessList raw = rawLivenessConditionToList 0 raw []

countAlive :: [Cell] -> Int
countAlive = length . filter (==Alive)

toLivenessFunction :: [Char] -> ([Cell] -> Cell)
toLivenessFunction raw cells = if
  or ((==) <$> toLivenessList raw <*> [countAlive cells])
  then Alive
  else Dead

-- map to new cells

makeNewCell :: (Int, Int) -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> Cell
makeNewCell coordinate cells aliveFunction birthFuntion =
  if mainCell == Alive
  then aliveFunction surroundingCells
  else birthFuntion surroundingCells
  where
    surroundingCells = getSurroundingCells coordinate cells
    mainCell = getMainCell coordinate cells

makeNewCellsRow :: [(Int, Int)] -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> [Cell]
makeNewCellsRow rowCoordinates cells aliveFunction birthFuntion =
  map (\coord -> makeNewCell coord cells aliveFunction birthFuntion) rowCoordinates

makeNewCells :: [[(Int, Int)]] -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> [[Cell]]
makeNewCells allCoordinates cells aliveFunction birthFuntion =
  map (\rowCoordinates -> makeNewCellsRow rowCoordinates cells aliveFunction birthFuntion) allCoordinates

-- join all

simulate :: Int -> Int -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> [[Cell]]
simulate h w cells aliveFunction birthFunction =
  makeNewCells coordinates cells aliveFunction birthFunction
  where
    coordinates = makeCoordinates h w

-- MAIN

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  [h, w, n] <- getHWN

  aliveRawCondition <- getRawLivenessCondition
  birthRawCondition <- getRawLivenessCondition
  initialRawCondition <- getRawInitialCondition h

  let aliveTransformationFunction = toLivenessFunction aliveRawCondition
  let birthTransformationFunction = toLivenessFunction birthRawCondition
  let cells = (addAllDeadCells . rawInitialConditionToCells) initialRawCondition


  print aliveRawCondition
  print birthRawCondition
  print $ toLivenessList birthRawCondition
  print $ makeCoordinates h w
  displayCells cells
  print $ getSurroundingCells (2,1) cells
  displayCells $ simulate h w cells aliveTransformationFunction birthTransformationFunction
  displayOutput $ simulate h w cells aliveTransformationFunction birthTransformationFunction

  return ()



  -- print (h, w, n)
  -- print aliveRawCondition
  -- print $ toLivenessList aliveRawCondition
  -- print $ toLivenessList birthRawCondition
  -- print birthRawCondition
  -- displayCells . addAllDeadCells . rawInitialConditionToCells $ initialRawCondition
  -- displayCells [getSurroundingCells (1, 1) ((addAllDeadCells . rawInitialConditionToCells) initialRawCondition)]
  -- print $ countAlive (getSurroundingCells (1, 1) ((addAllDeadCells . rawInitialConditionToCells) initialRawCondition))
  -- print $ toLivenessFunction "000000000" [Alive, Dead, Dead, Dead, Dead]
