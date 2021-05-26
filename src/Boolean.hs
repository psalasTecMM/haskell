module Boolean where

import Prelude hiding (not, and, or)


-- Data constructor not in scope: Day
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

fstDay :: (Eq a, Num a) => a -> Day
fstDay x
    | x == 1    = Sun
    | x == 2    = Mon
    | x == 3    = Tue
    | x == 4    = Wed
    | x == 5    = Thu
    | x == 6    = Fri
    | x == 7    = Sat

-- Sinonimos
type Hypotenusa = Integer
type Lado = Integer

triangulo :: Lado -> Lado -> Hypotenusa
triangulo a b = a + b

type Cbool a = a -> a -> a

-- >>> toZeroSequence 0.8 0.8
-- 1
toZeroSequence a b
    | a < b  = 0
    | a >= b = 1



--data Boolean = const | const id
-- >>> true 1 2
-- 1
true :: a -> a -> a
true = \ x y -> x

-- >>> false 1 2
-- 2
false :: a -> a -> a
false = \x y -> y

-- b = T -> T x y -> x = "True"
-- b = F -> F x y -> y = "False"

-- >>>printBoolean true
-- "True"
printBoolean :: Cbool String -> String
printBoolean = \b -> b "True" "False"


-- >>>printBoolean (not false)
-- "True"

-- >>> not false
-- No instance for (Show (Cbool b0)) arising from a use of ‘evalPrint’
--   (maybe you haven't applied a function to enough arguments?)
not :: Cbool(Cbool b) -> Cbool b
not = \b -> b false true

-- (\x y.x) -> ((x,y)-> x | y) -> (x -> y -> x|y) (x -> y -> y) (x -> y -> x)
-- (x -> y -> x|y)

-- >>> printBoolean (and true false)
-- "False"

and :: Cbool(Cbool b) -> Cbool b -> Cbool b
and = \b c -> b c false


-- >>> printBoolean (or false true)
-- "True"


-- >>> printBoolean (or true true)
-- "True"

-- >>> printBoolean (or false false)
-- "False"

or :: Cbool(Cbool b) -> Cbool b -> Cbool b
or = \b c -> b true c


-- >>> printBoolean (eq true true)
-- "True"
eq :: Cbool(Cbool(Cbool b)) -> Cbool(Cbool b) -> Cbool b
eq = \b c -> or (and b c) (and (not b) (not c))

-- b && c || not b && not c
