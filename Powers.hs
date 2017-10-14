module Powers where

type World = [[Int]]

showCell :: Int -> String
showCell 0 = ""
showCell n = show n

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

movingUpCoords = filter (getMovingTilesFilter Up) allCoords

at :: World -> (Int, Int) -> Int
at w (i, j) = w !! i !! j

isZeroPair w a b = w `at` a == 0 && w `at` b /= 0
isZeroTop w coords = isZeroPair w (getTowardsCoords Up coords) coords
isEqualPair w a b = w `at` a == w `at` b
isEqualUp w coords = isEqualPair w (getTowardsCoords Up coords) coords

moveUp :: World -> World
moveUp w = foldl swapZeroTops w zeroTopCoords where
    zeroTopCoords = find isZeroTop w movingUpCoords
    swapZeroTops :: World -> (Int, Int) -> World
    swapZeroTops w coords = swapTwoCells w (getTowardsCoords Up coords) coords
squashUp w = foldl squashEqual w equalCoords where
    equalCoords = find isEqualUp w movingUpCoords
    squashEqual w (i, j) = replaceCell (i-1, j) (2 * (w !! (i-1) !! j)) $
                           replaceCell (  i, j)  0 w

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

