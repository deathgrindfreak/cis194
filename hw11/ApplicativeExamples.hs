module ApplicativeExamples (mapA', sequenceA', replicateA) where

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
_ *> b = b

mapA' :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA' f = sequenceA' . map f

sequenceA' :: Applicative f => [f a] -> f [a]
sequenceA' = foldr (\x acc -> (:) <$> x <*> acc) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n a = sequenceA' $ replicate n a
