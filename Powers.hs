module Powers where

type World = [[Int]]

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

find :: (World -> a -> Bool) -> World -> [a] -> [a]
find p w = filter (p w)

allCoords = [(0,0), (0,1), (0,2), (0,3),
             (1,0), (1,1), (1,2), (1,3),
             (2,0), (2,1), (2,2), (2,3),
             (3,0), (3,1), (3,2), (3,3)]

movingUpCoords = filter (getMovingTilesFilter Up) allCoords

at :: World -> (Int, Int) -> Int
at w (i, j) = w !! i !! j

isZeroPair w a b = w `at` a /= 0 && w `at` b == 0
isZeroTop w coords = isZeroPair w coords (getTowardsCoords Up coords)
isEqualPair w a b = w `at` a == w `at` b
isEqualUp w coords = isEqualPair w coords (getTowardsCoords Up coords)

moveUp :: World -> World
moveUp w = foldl swapZeroTops w zeroTopCoords where
    zeroTopCoords = find isZeroTop w movingUpCoords
    swapZeroTops :: World -> (Int, Int) -> World
    swapZeroTops w coords = swapTwoCells w coords (getTowardsCoords Up coords)
squashUp w = foldl squashEqual w equalCoords where
    equalCoords = find isEqualUp w movingUpCoords
    squashEqual w coords = replaceCell stationary (2 * w `at` stationary) $
                           replaceCell coords 0 w where
        stationary = getTowardsCoords Up coords

data Dir = Up | Down | Left | Right

getMovingTilesFilter :: Dir -> ((Int, Int) -> Bool)
getMovingTilesFilter Up = \(i, _) -> i /= 0
getMovingTilesFilter _ = undefined

-- get the coordinates of tile towards which (i, j) tile will be moving in
-- specified direction
getTowardsCoords :: Dir -> (Int, Int) -> (Int, Int)
getTowardsCoords Up (i, j) = (i-1, j)
getTowardsCoords _ _ = undefined

up :: World -> World
up = squashUp . moveUp

update :: Char -> World -> World
update 'w' = up . up . up . up . up
-- todo for "asd" keys
update _ = id

