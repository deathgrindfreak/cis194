module Main where

-- splits an integer in to a list of integers in reverse
toDigitsRev :: Int -> [Int]
toDigitsRev x 
    | x `div` 10 == 0 = [x `rem` 10]
    | otherwise = (x `rem` 10) : toDigitsRev (x `div` 10)

-- splits an integer in to a list of integers
toDigits :: Int -> [Int]
toDigits = reverse . toDigitsRev

-- double every other integer in a list starting from the end
doubleAlt :: [Int] -> [Int]
doubleAlt x = dbl (even $ length x) x 
    where 
        dbl _ [] = []
        dbl b (x:xs)
            | b == True = (2 * x) : dbl False xs
            | otherwise = x : dbl True xs
            
-- Sum all digits in a list of Integers
sumAllDigs :: [Int] -> Int
sumAllDigs [] = 0
sumAllDigs (x:xs)
    | x `div` 10 == 0 = x + sumAllDigs xs
    | otherwise = (x `rem` 10)  + sumAllDigs ((x `div` 10) : xs)
            
-- Validats a credit card
validate = (== 0) . (`mod` 10) . sumAllDigs . doubleAlt . toDigits

main = do 
    print $ validate 4012888888881881