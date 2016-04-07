module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi d a b c =
    if d == 0 then [(a, b)]
    else hanoi (d - 1) a c b ++ [(a, b)] ++ hanoi (d - 1) c b a

main = do print $ hanoi 2 "a" "b" "c"
