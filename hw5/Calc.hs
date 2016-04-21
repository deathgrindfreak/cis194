{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as VM


-- Exercise 1

eval :: ExprT -> Integer
eval (Add (Lit a) (Lit b)) = a + b
eval (Add (Lit a) b) = a + eval b
eval (Add a (Lit b)) = eval a + b
eval (Mul (Lit a) (Lit b)) = a * b
eval (Mul (Lit a) b) = a * eval b
eval (Mul a (Lit b)) = eval a * b


-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
    Nothing -> Nothing
    Just e -> Just (eval e)
    
    
-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a
    
instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul
    

-- Exercise 4

instance Expr Integer where
    lit x = x
    add a b = a + b
    mul a b = a * b
    
instance Expr Bool where
    lit x | x >= 0 = False 
          | otherwise = True
    add a b = a || b
    mul a b = a && b
    
newtype MinMax = MinMax Integer deriving (Eq, Show)
    
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax a) (MinMax b) = MinMax (min a b)
    
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
    lit x = Mod7 (x `mod` 7)
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
    
testExpr :: Expr a => Maybe a
testExpr = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExpr :: Maybe Integer
testBool = testExpr :: Maybe Bool
testMinMax = testExpr :: Maybe MinMax
testMod7 = testExpr :: Maybe Mod7


-- Exercise 5

instance Expr VM.Program where
    lit x = [VM.PushI x]
    mul a b = a ++ b ++ [VM.Mul]
    add a b = a ++ b ++ [VM.Add]

compile :: String -> Maybe VM.Program
compile = parseExp lit mul add
