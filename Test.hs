module Main where

import System.Exit

import Powers

emptyRow = [0,0,0,0]
simpleWorld = [[ 2, 4, 0, 0], emptyRow, emptyRow, emptyRow]

testUpdate ch world expected = update ch world == expected

testUp = testUpdate 'w'

simpleTest = testUp simpleWorld simpleWorld
simpleUpTest = testUp
    [[2,0,0,0], [2,0,0,0], emptyRow, emptyRow]
    [[4,0,0,0],  emptyRow, emptyRow, emptyRow]

tests = [simpleTest, simpleUpTest]

fromTestResult :: Bool -> String
fromTestResult True = "Ok"
fromTestResult False = "Fail"

main = do
    let results = tests
    putStrLn $ show $ map fromTestResult tests
    if and tests then return ()
        else exitWith $ ExitFailure 1
