import System.IO
import Control.Monad

indexed :: [a] -> [(Int, a)]
indexed = go 0
  where
    go i (a:as) = (i, a) : go (i + 1) as
    go _ _      = []

type Cell = Char

alive :: Char
alive = 'O'
dead :: Char
dead = '.'

-- input

getRawInitialCondition :: Int -> IO [String]
getRawInitialCondition = flip replicateM getLine

-- output

printOutput :: [[Cell]] -> IO ()
printOutput = putStr . unlines

-- surrounding cells

getSurroundingCells :: (Int, Int) -> [[Cell]] -> [Cell]
getSurroundingCells (cx,cy) cells = do
  [cells !! y !! x | x <- [xo..xf], y <- [yo..yf], (x, y) /= (cx, cy)]
  where
      limitY = length cells - 1
      limitX = (length . head) cells - 1
      xo = max 0 (cx - 1)
      xf = min limitX (cx + 1)
      yo = max 0 (cy - 1)
      yf = min limitY (cy + 1)

getMainCell :: (Int, Int) -> [[Cell]] -> Cell
getMainCell (x,y) cells = cells !! y !! x

-- make coordinates

makeCoordinates :: Int -> Int -> [[(Int, Int)]]
makeCoordinates h w = [[(x,y) | x <- [0..xf]] | y <- [0..yf]]
    where
        xf = max 0 (w - 1)
        yf = max 0 (h - 1)

-- liveness function

toLivenessList :: [Char] -> [Int]
toLivenessList raw = map fst (filter (\(idx, char) -> char == '1') (indexed raw))

countAlive :: [Cell] -> Int
countAlive = length . filter (==alive)

toLivenessFunction :: [Char] -> ([Cell] -> Cell)
toLivenessFunction raw cells = if
  or ((==) <$> toLivenessList raw <*> [countAlive cells])
  then alive
  else dead

-- map to new cells

makeNewCell :: (Int, Int) -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> Cell
makeNewCell coordinate cells aliveFunction birthFuntion =
  if mainCell == alive
  then aliveFunction surroundingCells
  else birthFuntion surroundingCells
  where
    surroundingCells = getSurroundingCells coordinate cells
    mainCell = getMainCell coordinate cells

makeNewCells :: [[(Int, Int)]] -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> [[Cell]]
makeNewCells coordinates cells aliveFunction birthFunction =
  [
    [
      makeNewCell coord cells aliveFunction birthFunction
      | coord <- row
      ]
    | row <- coordinates
  ]

-- join all

simulate :: Int -> Int -> Int -> [[Cell]] -> ([Cell] -> Cell) -> ([Cell] -> Cell) -> [[Cell]]
simulate n h w cells aliveFunction birthFunction
  | n == 0 = cells
  | otherwise = simulate (n-1) h w newCells aliveFunction birthFunction
  where
    coordinates = makeCoordinates h w
    newCells = makeNewCells coordinates cells aliveFunction birthFunction

-- MAIN

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering -- DO NOT REMOVE

  [h, w, n] <- map (read :: String -> Int) . words <$> getLine

  aliveFunction <- toLivenessFunction <$> getLine
  birthFunction <- toLivenessFunction <$> getLine
  cells <- replicateM h getLine

  printOutput $ simulate n h w cells aliveFunction birthFunction

  return ()
