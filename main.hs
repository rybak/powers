import System.IO
import Data.Char (chr)
import Control.Concurrent (threadDelay)

import Text.Printf (printf)
import Data.List (intercalate)
import Control.Monad (join, when)
import Data.Function (fix)
import Data.Maybe (catMaybes)

type World = [[Int]]

showCell :: Int -> String
showCell 0 = ""
showCell n = show n

showLine :: [Int] -> String
showLine [a,b,c,d] = printf "|%5d%5d%5d%5d |" a b c d
showLine _ = undefined
boardTop = "+---------------------+" -- just ASCII for now
boardBottom = boardTop

renderWorld :: World -> IO ()
renderWorld world = do
    putStrLn $ intercalate "\n" $ boardTop : (map showLine world) ++ [boardBottom]

goodInput = "wasd"

replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x = prefix ++ (x : postfix) where
    prefix = take i xs
    postfix = drop (i + 1) xs

swapTwoCells :: World -> (Int, Int) -> (Int, Int) -> World
swapTwoCells w (i1, j1) (i2, j2) = replaceElement (replaceElement w i1 newLine1) i2 newLine2 where
    newLine1 = replaceElement (w !! i1) j1 (w !! i2 !! j2)
    newLine2 = replaceElement (w !! i2) j2 (w !! i1 !! j1)

replaceCell :: (Int, Int) -> Int -> World -> World
replaceCell (i, j) x w = replaceElement w i newLine where
    newLine = replaceElement (w !! i) j x

find :: (World -> (Int, Int) -> Bool) -> World -> [(Int, Int)] -> [(Int, Int)]
find p w = filter (p w)

allCoords = [(0,0), (0,1), (0,2), (0,3),
             (1,0), (1,1), (1,2), (1,3),
             (2,0), (2,1), (2,2), (2,3),
             (3,0), (3,1), (3,2), (3,3)]

verticalUp = filter (\(i,_) -> i /= 0) allCoords

isZeroTop w (i, j) = ((w !! (i-1) !! j) == 0) && ((w !! i !! j) /= 0)

moveUp :: World -> World
moveUp w = foldl swapZeroTops w zeroTopCoords where
    zeroTopCoords = find isZeroTop w verticalUp
    swapZeroTops :: World -> (Int, Int) -> World
    swapZeroTops w (i, j) = swapTwoCells w (i-1, j) (i, j)
squashUp w = foldl squashEqual w equalCoords where
    equalCoords = find isEqualUp w verticalUp
    squashEqual w (i, j) = replaceCell (i-1, j) (2 * (w !! (i-1) !! j)) $
                           replaceCell (  i, j)  0 w

isEqualUp w (i, j) = (w !! (i-1) !! j) == (w !! i !! j)

up :: World -> World
up = squashUp . moveUp

update :: Char -> World -> World
update 'w' = up . up . up . up . up
-- todo for "asd" keys
update _ = id

gameLoop :: Handle -> World -> IO ()
gameLoop i w = go True i w where
    go needRender input world = do
        when needRender $ renderWorld world
        e <- threadDelay 2000
        ch <- readAll input ' '
        when (ch /= 'q') $ go (ch `elem` goodInput) input $ update ch world

readAll :: Handle -> Char -> IO (Char)
readAll h ch = do
    gotIt <- hReady h 
    if gotIt
        then hGetChar h >>= readAll h
        else return ch

initial = [[   2, 0, 0, 32],
           [ 128, 0, 2,  0], 
           [   0, 4, 2, 32],
           [  16, 8, 0,  0]]

main = do
    hSetBuffering stdin NoBuffering --get input immedietly
    hSetBuffering stdout NoBuffering 
    hSetEcho stdin False --don't show the typed character
    gameLoop stdin initial
