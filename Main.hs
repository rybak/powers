module Main where

import Prelude hiding (Left, Right)
import System.IO
import Data.Char (chr)
import Control.Concurrent (threadDelay)

import Text.Printf (printf)
import Data.List (intercalate)
import Control.Monad (join, when)
import Data.Function (fix)
import Data.Maybe (catMaybes)

import Powers

showCell :: Int -> String
showCell 0 = "     "
showCell n = printf "%5d" n

showLine :: [Int] -> String
showLine line = "|" ++ (concat $ map showCell line) ++ " |"

boardTop = "+---------------------+" -- just ASCII for now
boardBottom = boardTop

renderWorld :: World -> IO ()
renderWorld world = do
    putStrLn $ intercalate "\n" $ boardTop : (map showLine world) ++ [boardBottom]

charToDir :: Char -> Maybe Dir
charToDir 'w' = Just Up
charToDir 'a' = Just Left
charToDir 's' = Just Down
charToDir 'd' = Just Right
charToDir _   = Nothing

gameLoop :: Handle -> World -> IO ()
gameLoop i w = go True i w where
    go needRender input world = do
        when needRender $ renderWorld world
        e <- threadDelay 2000
        ch <- readAll input ' '
        when (ch /= 'q') $ case charToDir ch of
            Just dir -> go True  input $ update dir world
            Nothing  -> go False input world

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
    hSetBuffering stdin NoBuffering --get input immediately
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False --don't show the typed character
    gameLoop stdin initial
