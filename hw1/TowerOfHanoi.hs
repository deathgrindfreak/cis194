module Main where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi p a b c = undefined

main = do print $ hanoi 2 "a" "b" "c"
