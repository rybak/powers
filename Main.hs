module Main where

import System.IO
import Data.Char (chr)
import Control.Concurrent (threadDelay)

import Text.Printf (printf)
import Data.List (intercalate)
import Control.Monad (join, when)
import Data.Function (fix)
import Data.Maybe (catMaybes)

import Powers


showLine :: [Int] -> String
showLine [a,b,c,d] = printf "|%5d%5d%5d%5d |" a b c d
showLine _ = undefined
boardTop = "+---------------------+" -- just ASCII for now
boardBottom = boardTop

renderWorld :: World -> IO ()
renderWorld world = do
    putStrLn $ intercalate "\n" $ boardTop : (map showLine world) ++ [boardBottom]

goodInput = "wasd"

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
