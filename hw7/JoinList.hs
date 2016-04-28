module JoinList where

import Data.Monoid
import Sized
import Scrabble


(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

yeah :: JoinList Size Char
yeah = Append (Size 4) 
        (Append (Size 3) 
            (Single (Size 1) 'y') 
            (Append (Size 2) 
                (Single (Size 1) 'e') 
                (Single (Size 1) 'a'))) 
        (Single (Size 1) 'h')
        
fucker :: JoinList Size Char
fucker = Append (Size 6)
            (Append (Size 3)
                (Single (Size 1) 'f')
                (Append (Size 2)
                    (Single (Size 1) 'u')
                    (Single (Size 1) 'c')))
            (Append (Size 3)
                (Single (Size 1) 'k')
                (Append (Size 2)
                    (Single (Size 1) 'e')
                    (Single (Size 1) 'r')))


-- Exercise 1

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a  
(+++) a b = Append (ta `mappend` tb) a b
    where ta = tag a
          tb = tag b

-- Exercise 2

jSize :: (Sized b, Monoid b) => JoinList b a -> Int
jSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Single _ _) = Nothing
indexJ i (Append _ l r)
    | i < s     = indexJ i l
    | otherwise = indexJ (i - s) r 
    where s = jSize l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ 0 j            = j
dropJ i (Single _ _) = Empty
dropJ i (Append _ l r)
    | i < s     = dropJ i l +++ r
    | otherwise = dropJ (i - s) r 
    where s = jSize l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty        = Empty
takeJ 0 _        = Empty
takeJ i s@Single{} = s
takeJ i (Append _ l r)
    | i < s     = takeJ i l 
    | otherwise = l +++ takeJ (i - s) r
    where s = jSize l
    

-- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s