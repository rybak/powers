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

gameLoop :: RandomGen g => Handle -> g -> World -> IO ()
gameLoop i g w = do
    go g True i w where
    go g needRender input world = do
        when needRender $ renderWorld world
        e <- threadDelay 2000
        ch <- readAll input ' '
        when (ch /= 'q') $ case charToDir ch of
            Just dir -> go nextG True  input newWorld where
                (nextG, newWorld) = update g dir world
            Nothing  -> go g     False input world

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
    hSetBuffering stdin NoBuffering --get input immediately
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False --don't show the typed character
    g <- getStdGen
    gameLoop stdin g initial
