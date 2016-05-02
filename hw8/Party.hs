module Party where

import Data.Monoid
import Employee


-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL es1 f1) (GL es2 f2) = GL (es1 ++ es2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ f1) r@(GL _ f2) | f1 < f2  = r
                                | f2 >= f2 = l
