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
import System.Random
import System.Environment (getArgs)

import Powers

type Render = World -> IO ()
renderAsciiLines :: String -> String -> ([Int] -> String) -> String -> Render
renderAsciiLines top bottom renderLine sep world = do
    putStrLn top
    putStrLn $ intercalate sep $ map renderLine world
    putStrLn bottom
    putStrLn ""

renderAsciiSimple :: Render
renderAsciiSimple = renderAsciiLines simpleLine simpleLine showLine "\n" where
    simpleLine = "+---------------------+"
    showCell :: Int -> String
    showCell 0 = "     "
    showCell n = printf "%5d" n
    showLine :: [Int] -> String
    showLine line = "|" ++ (concat $ map showCell line) ++ " |"

renderAsciiGrid :: Render
renderAsciiGrid = renderAsciiLines gridLine gridLine showLineGrid sep where
    gridLine = "+-------+-------+-------+-------+"
    sep = "\n" ++ gridLine ++ "\n"
    showLineGrid line = '|' : (concat $ map showCellGrid line) where
        showCellGrid 0 = "       |"
        showCellGrid n = printf "%6d |" n

charToDir :: Char -> Maybe Dir
charToDir 'w' = Just Up
charToDir 'a' = Just Left
charToDir 's' = Just Down
charToDir 'd' = Just Right
charToDir _   = Nothing

gameLoop :: RandomGen g => Handle -> g -> World -> Render -> IO ()
gameLoop i g w render = go g True i w where
    go g needRender input world = do
        when needRender $ render world
        if gameOver world
        then putStrLn "Game over!"
        else do
            e <- threadDelay 2000
            ch <- readAll input ' '
            when (ch /= 'q') $ case charToDir ch of
                Just dir -> go nextG True  input newWorld where
                    (nextG, newWorld) = update g dir world
                Nothing  -> go g     False input world

gameOver :: World -> Bool
gameOver world = a == b && b == c && c == d where
    a = step Up world
    b = step Down world
    c = step Left world
    d = step Right world

readAll :: Handle -> Char -> IO (Char)
readAll h ch = do
    gotIt <- hReady h
    if gotIt
        then hGetChar h >>= readAll h
        else return ch

initial  = [[ 0, 2, 0, 0],
            [ 2, 0, 0, 0],
            [ 0, 0, 0, 0],
            [ 0, 0, 0, 0]]

main = do
    args <- getArgs
    hSetBuffering stdin NoBuffering --get input immediately
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False --don't show the typed character
    g <- getStdGen
    let r = if null args then renderAsciiSimple else renderAsciiGrid
    gameLoop stdin g initial r
