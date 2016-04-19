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
    
height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

calcH :: Tree a -> Integer
calcH Leaf = 0
calcH (Node _ left _ right) = 1 + max (calcH left) (calcH right)
    
foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Leaf
    where insert x Leaf = Node 0 Leaf x Leaf
          insert x (Node h left n right)
            | hl < hr = Node h (insert x left) n right
            | hl > hr = Node h left n (insert x right)
            | otherwise = let t@(Node _ l nn r) = Node h (insert x left) n right
                          in Node (calcH t - 1) l nn r
            where hl = height left
                  hr = height right
            

-- Exercise 3

xor :: [Bool] -> Bool
xor = foldr (\x b -> if x then not b else b) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []


-- Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let f i j = i + j + 2 * i * j
                      fil = [f i j | i <- [1..n], j <- [1..n], f i j <= n] in
    (map (\x -> 2 * x + 1) . filter (`notElem` fil)) [1..n]
