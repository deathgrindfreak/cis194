module Golf where

import Data.List

-- Exercise 1

-- Takes an integer n and a list of elements and returns a list of every nth element
skipN :: Int -> [a] -> [a]
skipN n = map snd . filter (\(i, _) -> i `mod` n == 0) . zip [1..]

-- Takes a list and returns a list of lists consisting of all the ith elements
-- from i <- [1..(length list)]
skip :: [a] -> [[a]]
skip l = map ((flip skipN) l) [1..(length l)]


-- Exercise 2

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima [x] = []
localMaxima (x:y:[]) = []
localMaxima (x:y:z:xs)
    | x < y && z < y = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
    
    
-- Exercise 3

hist :: [Int] -> [(Int, Int)]
hist = map (\l@(x:_) -> (x, length l)) . groupBy (==) . sort

{-
histStr :: [(Int, Int)] -> String
histStr l = let m = maximum . map snd l
                wh w = map (\_ -> ' ') [1..w] in 
                undefined
    --intercalate "\n" $ transpose $ 
-}
