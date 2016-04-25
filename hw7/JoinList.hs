module JoinList where

import Sized

-- Exercise 1

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a  
(+++) a b = Append (ta `mappend` tb) a b
    where ta = tag a
          tb = tag b

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Single _ _) = Nothing
indexJ 0 (Append _ _ _) = Nothing
