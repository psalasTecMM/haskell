module ArreglosTuplas where

import Prelude hiding (fst,snd,length, head, tail)

-- Tuples

-- >>> fst (3,4)
-- 3

fst :: (a, b) -> a
fst (x,y) = x

-- >>> snd (3,4)
-- 4

snd :: (a, b) -> b
snd (x,y) = y

-- >>>()
-- ()
type N = String
type Sigma = String
type P = (Char, String)
type S = Char

type Grammar = (N,Sigma,P,S)

-- CYK (Grammar g)

type Point = (Integer, Integer)

suma :: (Point,Point) -> Point
suma (x,y) = (fst x + fst y , snd x + snd y)

{-

4|
3|      *
2|  *
1|    *
0|___________
  0 1 2 3 4 5

-}

-- >>> suma ((1,2),(2,1))
-- (3,3)

-- List

--[] List definition
--[Char]
--[Int]


range = [1..10]

-- >>> range
-- [1,2,3,4,5,6,7,8,9,10]

oddNumbers = [1,3..10]

-- >>> oddNumbers
-- [1,3,5,7,9]

-- Colon Operator
-- (:) :: a -> [a] -> [a]
-- >>> 1 : [2,3,4,5]
-- [1,2,3,4,5]

-- >>> 'h' : []
-- "h" --> [Char] -> String


-- >>> potencia 5 "hola"
-- "holaholaholaholahola"

-- w^0 = ""
-- w^k+1 = w * w^k

potencia :: (Eq t, Num t) => t -> [a] -> [a]
potencia 0 _ = [] -- Base
potencia n x = x ++ potencia (n-1) x -- Recursivo

-- (x:xs) -> ["hola"] -> ("h":["ola"])
-- ["hola"] -> ["hola"] ++ ["hola"]
-- potencia 5 ["hola"] -> ["hola"] ++ ["holaholaholahola"] -> ["holaholaholaholahola"]
-- potencia 4 ["hola"] -> ["hola"] ++ ["holaholahola"] -> ["holaholaholahola"]
-- potencia 3 ["hola"] -> ["hola"] ++ ["holahola"] -> ["holaholahola"]
-- potencia 2 ["hola"] -> ["hola"] ++ ["hola"] -> ["holahola"]
-- potencia 1 ["hola"] -> ["hola"] ++ "" -> ["hola"]
-- potencia 0 ["hola"] ->  ""

-- [] -> Integer
-- >>> length (potencia 5 "hola")
-- Variable not in scope: potencia :: t0 -> [Char] -> [a0]
length [] = 0
length (x:xs) = 1 + length xs

-- lenght [1,2,3,4,5] = 1 + 4 = 5
-- lenght [2,3,4,5] = 1 + 3 = 4
-- lenght [3,4,5] = 1 + 2 = 3
-- lenght [4,5] = 1 + 1 = 2
-- lenght [5] = 1 + 0 = 1
-- lenght [] = 0

newArray = [1,2,3,4] ++ [5]
-- >>> newArray
-- [1,2,3,4,5]

-- (++) Operator
-- (++) :: [a] -> [a] -> [a]

-- >>> "Hola " ++ "Mundo" --> [Char] ++ [Char] -> [Char]
-- "Hola Mundo"

-- (!!) Operator
-- (!!) :: [a] -> Int -> a

-- >>> [1,2,3,4,5] !! 3
-- 4

-- >>> extract [1,2,3,4,5] 5
-- -1

extract [] n = -1
extract (x:_) 0 = x
extract (x:xs) n = extract xs (n-1)

-- extract [1,2,3,4,5] 3 = extract [2,3,4,5] n
-- extract [2,3,4,5] 2 = extract [3,4,5] n
-- extract [3,4,5] 1 = extract [4,5] n
-- extract [4,5] 0 = x

-- head & tail

-- >>> head [1,2,3,4,5]
-- 1

head (x:xs) = x

-- >>> tail [1,2,3,4,5]
-- [2,3,4,5]
tail (x:xs) = xs

-- elem :: Eq a => a -> [a] -> Bool

-- >>> elem 6 [1,2,3,4,5]
-- False

-- >>> elem2 5 [1,2,3,4,5]
-- True
elem2 y [] = False
elem2 y (x:xs) = if y == x then True else elem2 y xs

-- maximum, minimum, sum, product ???

-- >>> maximum' [10,7,3,8,5]
-- 10
maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y ->if x >= y then x else y)

-- >>> minimum' [10,7,3,8,5]
-- 3

minimum' :: Ord a => [a] -> a
minimum' = foldr1 (\x y ->if x < y then x else y)

-- >>> sum' [1,2,3,4,5]
-- 15

sum' :: (Eq a , Num a) => [a] -> a
sum' = foldr1 (\x y -> x + y )

-- >>> prod' [1,2,3,4,5]  5!
-- 120

prod' :: (Eq a , Num a) => [a] -> a
prod' = foldr1 (\x y -> x * y )

-- JavaScript -- map, filter
-- Java -- map, filter
-- C/C++ -- map, filter
-- python -- map , filter

-- >>> max' [1,10,8,4,5]
-- 10

max' (x:y:xs) = if x >= y then max' (x:xs) else max' (y:xs)
max' x = head x 

-- >>> min' [1,10,8,4,5]
-- 1
min' (x:y:xs) = if x < y then min' (x:xs) else min' (y:xs)
min' x = head x

-- >>> sum2 [1,2,3,4,5]
-- 15

sum2 (x:y:xs) = sum2 ((x+y):xs)
sum2 x = head x

-- >>> prod2 [1,2,3,4,5]
-- 120

prod2 (x:y:xs) = prod2 ((x*y):xs)
prod2 x = head x
