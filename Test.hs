module Main where

import System.Exit

import Powers

emptyRow = [0,0,0,0]
simpleWorld = [[ 2, 4, 0, 0], emptyRow, emptyRow, emptyRow]

testStep ch world expected = step ch world == expected

testUp = testStep Up
testDown = testStep Down

simpleTest = testUp simpleWorld simpleWorld
simpleUpTest = testUp
    [[2,0,0,0], [2,0,0,0], emptyRow, emptyRow]
    [[4,0,0,0],  emptyRow, emptyRow, emptyRow]

simpleTest2 = testDown simpleWorld (reverse simpleWorld)
simpleDownTest = testDown
    [[2,0,0,0], [2,0,0,0], emptyRow,  emptyRow]
    [ emptyRow,  emptyRow, emptyRow, [4,0,0,0]]

upFilled = testUp [[   2, 0, 0, 32],
                   [ 128, 0, 2,  0],
                   [   0, 4, 2, 32],
                   [  16, 8, 0,  0]]

                  [[   2, 4, 4, 64],
                   [ 128, 8, 0,  0],
                   [  16, 0, 0,  0],
                   [   0, 0, 0,  0]]

downFilled = testDown [[ 2, 0,  0, 2],
                       [ 4, 0,  8, 0],
                       [ 8, 4,  8, 2],
                       [ 0, 8,  0, 0]]

                      [[ 0, 0,  0, 0],
                       [ 2, 0,  0, 0],
                       [ 4, 4,  0, 0],
                       [ 8, 8, 16, 4]]


tests = [
    simpleTest,
    simpleUpTest,
    upFilled,
    simpleTest2,
    simpleDownTest,
    downFilled
    ]

fromTestResult :: Bool -> String
fromTestResult True = "Ok"
fromTestResult False = "Fail"

main = do
    let results = tests
    putStrLn $ show $ map fromTestResult tests
    if and tests then return ()
        else exitWith $ ExitFailure 1
