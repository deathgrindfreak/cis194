{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

import Data.List

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]


-- Exercise 2

fibs2 :: [Integer]
fibs2 = f 0 1  where f a b = a : f b (a + b)


-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a xs) = a : streamToList xs

instance Show a => Show (Stream a) where
    show = foldr showCons [] . take 20 . streamToList
        where showCons x a = "Cons " ++ show x ++ case a of 
                [] -> " ..." 
                _  -> " (" ++ a
                

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a xs) = Cons (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))


-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interLeaveStreams :: Stream a -> Stream a -> Stream a
interLeaveStreams (Cons a as) (Cons b bs) = Cons a (Cons b (interLeaveStreams as bs))

ruler :: Stream Integer
ruler = interLeaveStreams (streamRepeat 0) (interLeaveStreams (streamRepeat 1) (streamMap (+2) nats))


-- Exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger a = Cons a (streamRepeat 0)
    negate = streamMap (*(-1))
    (+) (Cons a as) (Cons b bs) = Cons (a + b) ((+) as bs)
    (*) (Cons a as) bx@(Cons b bs) = Cons (a * b) ((streamMap (*a) bs) + (as * bx))
    abs = undefined
    signum = undefined
    
instance Fractional (Stream Integer) where
    (/) a@(Cons ae as) b@(Cons be bs) = Cons (ae `div` be) (streamMap ((1 `div` be)*)  (as - bs * (a / b)))
    recip = undefined
    fromRational = undefined
    
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- Exercise 7

data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
    show (Matrix a b c d) = "|" ++ show a ++ " " ++ show b ++ "|\n|" ++ show c ++ " " ++ show d ++ "|"

instance Num Matrix where
    fromInteger a = Matrix a a a a
    negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
    (+) (Matrix a b c d) (Matrix e f g h) = Matrix (a + e) (b + f) (c + g) (d + h)
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)
    abs = undefined
    signum = undefined
    
second :: Matrix -> Integer
second (Matrix _ a _ _) = a
    
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = second m where m = (Matrix 1 1 1 0) ^ n