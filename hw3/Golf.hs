module Golf where

import Data.List
import Data.Ord

-- Exercise 1

-- Takes an integer n and a list of elements and returns a list of every nth element
skipN :: Int -> [a] -> [a]
skipN n = map snd . filter (\(i, _) -> i `mod` n == 0) . zip [1..]

-- Takes a list and returns a list of lists consisting of all the ith elements
-- from i <- [1..(length list)]
skip :: [a] -> [[a]]
skip l = map (`skipN` l) [1..(length l)]


-- Exercise 2

localMaxima :: [Int] -> [Int]
localMaxima [] = []
localMaxima [x] = []
localMaxima [x,y] = []
localMaxima (x:y:z:xs)
    | x < y && z < y = y : localMaxima (y:z:xs)
    | otherwise = localMaxima (y:z:xs)
    
    
-- Exercise 3

hist :: [Int] -> [(Int, Int)]
hist = map (\l@(x:_) -> (x, length l)) . group . sort

addZ :: [(Int, Int)] -> [(Int, Int)]
addZ l = let m = map (\x -> (x, 0)) $ filter (`notElem` map fst l) [0..9]
         in m ++ l

histStr :: [(Int, Int)] -> String
histStr l = let m = (maximum . map snd) l 
                f =  map (\(n, x) -> replicate (m - x) ' ' ++ replicate x '*' 
                                        ++ "=" ++ show n) l
                in intercalate "\n" $ transpose f

histogram :: [Int] -> String
histogram = histStr . sortBy (comparing fst) . addZ . hist
