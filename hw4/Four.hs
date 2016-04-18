module Four where

-- Exercise 1
    
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs
       
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else 3*n + 1)


-- Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
foldTree :: Ord a => [a] -> Tree a
foldTree l = foldr (insert 0) Leaf l
    where insert xh x Leaf = Node xh Leaf x Leaf
          insert xh x (Node h left n right)
            | x < n = Node h (insert (xh+1) x left) n right
            | x > n = Node h left n (insert (xh+1) x right)
            

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\x b -> case x of True -> not b; False -> b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl = undefined


-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let f i j = i + j + 2 * i * j
                      fil = [f i j | i <- [1..n], j <- [1..n], f i j <= n] in
    (map (\x -> 2 * x + 1) . filter (`notElem` fil)) [1..n]