module JoinList where

import Data.Monoid
import Sized

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- Exercise 1

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a  
(+++) a b = Append (ta `mappend` tb) a b
    where ta = tag a
          tb = tag b

-- Exercise 2

jSize :: Sized b => JoinList b a -> Int
jSize = getSize . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Single _ _) = Nothing
indexJ i (Append _ l r)
    | jSize l > i = indexJ (i - 1) l
    | otherwise  = indexJ (i - 1) r 
