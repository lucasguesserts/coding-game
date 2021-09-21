import System.IO
import Control.Monad


firstHalve :: [a] -> [a]
firstHalve xs = case xs of
    [] -> []
    xs -> take (length xs `div` 2 ) xs

secondHalve :: [a] -> [a]
secondHalve xs = case xs of
    [] -> []
    xs -> drop (length xs `div` 2 ) xs

halve :: [a] -> ([a],[a])
halve xs = case xs of
    [] -> ([],[])
    xs -> (firstHalve xs, secondHalve xs)


type Note = [[[Char]]]

inputToNote :: [[Char]] -> Note
inputToNote = map (map (: []))

rightToLeft :: Note -> Note
rightToLeft = map (\row -> (map (\(left, right) -> reverse right ++ left) . uncurry zip . (\(f,s) -> (reverse f, s))) (halve row))

leftToRight :: Note -> Note
leftToRight = map (\row -> (map (\(left, right) -> reverse left ++ right) . uncurry zip . (\(f,s) -> (f, reverse s))) (halve row))

bottomToTop :: Note -> Note
bottomToTop note = (map (\(top, bottom) -> map (\(t, b) -> reverse b ++ t)(zip top bottom)) . uncurry zip . (\(f,s) -> (f, reverse s))) (halve note)

topToBottom :: Note -> Note
topToBottom note = (map (\(top, bottom) -> map (\(t, b) -> reverse t ++ b)(zip top bottom)) . uncurry zip . (\(f,s) -> (reverse f, s))) (halve note)

noteDone :: Note -> Bool
noteDone note = length note == 1

accNotes :: [Note -> Note] -> Note -> [Note]
accNotes fds init = scanl (\note fd -> fd note) init fds

doIt :: [Note -> Note] -> Note -> Note
doIt fds init = head (dropWhile (not . noteDone) (accNotes fds init))

toOutput :: Note -> String
toOutput = concat . concat

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    n <- fmap read getLine :: IO Int
    hPrint stderr n

    input <- replicateM n getLine
    let encodedMessage = map (filter (/=' ')) input

    let encodedNote = inputToNote encodedMessage

    -- hPrint stderr input
    -- hPrint stderr encodedMessage
    hPrint stderr encodedNote
    hPrint stderr $ rightToLeft encodedNote
    hPrint stderr $ (bottomToTop . rightToLeft) encodedNote
    hPrint stderr $ (leftToRight . bottomToTop . rightToLeft) encodedNote
    hPrint stderr $ (topToBottom . leftToRight .bottomToTop . rightToLeft) encodedNote
    hPrint stderr $ (rightToLeft . topToBottom . leftToRight .bottomToTop . rightToLeft) encodedNote
    hPrint stderr $ (bottomToTop . rightToLeft . topToBottom . leftToRight .bottomToTop . rightToLeft) encodedNote

    let folds = cycle [rightToLeft, bottomToTop, leftToRight, topToBottom]
    let x = accNotes folds encodedNote
    -- hPrint stderr x

    putStrLn $ toOutput $ doIt folds encodedNote
    return ()
