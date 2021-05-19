module ChurchNumerals (zero) where

import Prelude hiding (succ)

type Church a = (a->a) -> a -> a
-- >>> zero 1 2
-- 2
zero:: (a->a) -> a -> a
zero = \ x y -> y


-- >>> one id 2
-- 2
one :: (a->a) -> a -> a
one = \x y -> x y

-- >>>k 1 2
-- 1
k = \ x y -> x

i = \x -> x

{-
ki = (x y -> x) (x -> x)
   = (y ->(x->x)) = (y->x->x)
>>> ki 1 2
2
-}

ki = \x y -> k i x y


-- >>>:t succ(zero)
-- succ(zero) :: forall t1 t2. (t1 -> t2) -> t1 -> t2
-- 
succ :: Church a -> Church a
succ = \n x y -> x (n x y)

-- >>> churchToInt zero
-- 0
churchToInt :: Church Integer -> Integer
churchToInt = \n -> n (\x -> x + 1) 0

-- >>> :t intToChurch 4
-- intToChurch 4 :: forall t2. (t2 -> t2) -> t2 -> t2

-- >>> three = intToChurch 3
-- >>> :t three
-- >>> churchToInt three
-- three :: forall a. (a -> a) -> a -> a
-- 3
intToChurch:: Integer -> Church a
intToChurch 0 = \ x y -> y
intToChurch n = \ x y -> x (intToChurch (n-1) x y)

