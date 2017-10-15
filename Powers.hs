module Powers where

import Prelude hiding (Left, Right)
import System.Random

type World = [[Int]]
type Coords = (Int, Int)

createPair :: (a -> a) -> a -> (a, a)
createPair f a = (a, f a)

replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs i x = prefix ++ (x : postfix) where
    prefix = take i xs
    postfix = drop (i + 1) xs

replaceCell :: Coords -> Int -> World -> World
replaceCell (i, j) x w = replaceElement w i newLine where
    newLine = replaceElement (w !! i) j x

find :: (World -> a -> Bool) -> World -> [a] -> [a]
find p w = filter (p w)

allCoords = [(0,0), (0,1), (0,2), (0,3),
             (1,0), (1,1), (1,2), (1,3),
             (2,0), (2,1), (2,2), (2,3),
             (3,0), (3,1), (3,2), (3,3)]

movingPairs :: Dir -> [(Coords, Coords)]
movingPairs dir = filter (\(a, b) -> isValid a && isValid b) $
    map (createPair (getTowardsCoords dir)) allCoords
isValid :: Coords -> Bool
isValid (i,j) = valid i && valid j where
    valid z = 0 <= z && z <= 3

at :: World -> Coords -> Int
at w (i, j) = w !! i !! j

isZeroPair :: World -> Coords -> Coords -> Bool
isZeroPair w a b = w `at` a /= 0 && w `at` b == 0
isEqualPair :: World -> Coords -> Coords -> Bool
isEqualPair w a b = w `at` a == w `at` b

move :: Dir -> World -> World
move dir w = foldl swap w zeroTopPairs where
    zeroTopPairs :: [(Coords, Coords)]
    zeroTopPairs = find (uncurry . isZeroPair) w $ movingPairs dir
    swap :: World -> (Coords, Coords) -> World
    swap w (a,b) = replaceCell a y $ replaceCell b x w where
        x = w `at` a
        y = w `at` b
squash dir w = foldl squash w equalPairs where
    equalPairs = find (uncurry . isEqualPair) w $ movingPairs dir
    squash :: World -> (Coords, Coords) -> World
    squash w (a,b) = replaceCell a 0 $ replaceCell b sum w where
        sum = w `at` a + w `at` b

data Dir = Up | Down | Left | Right

getMovingTilesFilter :: Dir -> (Coords -> Bool)
getMovingTilesFilter Up    = \(i, _) -> i /= 0
getMovingTilesFilter Down  = \(i, _) -> i /= 3
getMovingTilesFilter Left  = \(_, j) -> j /= 0
getMovingTilesFilter Right = \(_, j) -> j /= 3

-- get the coordinates of tile towards which (i, j) tile will be moving in
-- specified direction
getTowardsCoords :: Dir -> Coords -> Coords
getTowardsCoords Up    (i, j) = (i-1, j)
getTowardsCoords Down  (i, j) = (i+1, j)
getTowardsCoords Left  (i, j) = (i, j-1)
getTowardsCoords Right (i, j) = (i, j+1)

step :: Dir -> World -> World
step dir = m . m . m . m . (squash dir) . m . m . m . m where
    m = move dir

addNew :: RandomGen g => g -> World -> (g, World)
addNew = (,)

update :: RandomGen g => g -> Dir -> World -> (g, World)
update g d = addNew g . step d
