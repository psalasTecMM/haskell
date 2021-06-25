module Arboles (Tree, leaf, branch , val, left, right, isLeaf, prefix, postfix, infix1) where

{-
      *
   3       +
        1     2

 Prefix * 3 + 1 2
 Infix 3 * 1 + 2
 Postfix 3 1 2 + *

-}

-- Recursive Types
data Tipo = Spade | Club | Heart | Diamond
type Cantidad = Int
data CardType = CardType Tipo Cantidad

spades = CardType Spade 13
hearts = CardType Heart 13

type Name = String
type Year = Int
type Persona = (Name, Year)

-- >>> (1992,"Aldo") :: Persona
-- Couldn't match type ‘[Char]’ with ‘Int’
-- Expected type: Year
--   Actual type: [Char]

data Person = Person
    { personFirstName :: Name
    , personLastName :: Name
    , yearOfBirth :: Year }
  deriving (Show, Eq, Ord)

-- >>> Person { yearOfBirth = 1992, personLastName ="Martinez", personFirstName = "Aldo" }
-- Person {personFirstName = "Aldo", personLastName = "Martinez", yearOfBirth = 1992}

-- >>> Person "Martinez" 1992 "Aldo" 
-- Couldn't match type ‘[Char]’ with ‘Int’
-- Expected type: Year
--   Actual type: [Char]

data Producto

type Inventario = [Producto]

people :: [Person]
people =
  [ Person "Isaac" "Newton" 1643
  , Person "Leonard" "Euler" 1707
  , Person "Blaise" "Pascal" 1623
  , Person "Ada" "Lovelace" 1815
  , Person "Alan" "Turing" 1912
  , Person "Haskell" "Curry" 1900
  , Person "John" "von Neumann" 1903
  , Person "Lipot" "Fejer" 1880
  , Person "Grace" "Hopper" 1906
  , Person "Anita" "Borg" 1949
  , Person "Karen" "Sparck Jones" 1935
  , Person "Henriette" "Avram" 1919 ]

llenado :: Name -> Name -> Year -> Person
llenado name apellido año = Person name apellido año

-- >>> llenado "Aldo" "Martinez" 1992
-- Person {personFirstName = "Aldo", personLastName = "Martinez", yearOfBirth = 1992}

add :: Inventario -> Producto -> Producto
add table item = item

data Tree a = Leaf a | Branch a (Tree a) (Tree a)

data Treen a = Leafn a | Branchn a [Treen a]

leaf = Leaf
branch = Branch
val (Leaf a) = a
val (Branch a l r) = a
left (Branch a l r) = l
right (Branch a l r) = r
isLeaf (Leaf _) = True 
isLeaf _ = False


arbol = Branch '*' (Leaf '3') (Branch '+' (Leaf '1') (Leaf '2'))
-- >>> prefix arbol
-- "* 3 + 1  2 "

prefix :: Tree a -> [a]
prefix (Leaf x) = [x]
prefix (Branch x l r) =  [x] ++ prefix l ++ prefix r

-- >>> postfix arbol
-- " 3  1  2 +*"

postfix :: Tree a -> [a]
postfix (Leaf x) = [x]
postfix (Branch x l r) = postfix l ++ postfix r ++ [x]

-- >>> infix1 arbol
-- " 3 * 1 + 2 "

infix1 :: Tree a -> [a]
infix1 (Leaf x) = [x]
infix1 (Branch x l r) = infix1 l ++ [x] ++ infix1 r 

-- Eq define igualdad == /=
-- Ord define igualdad < > <= >=

instance (Eq a) => Eq (Tree a) where
    Leaf a == Leaf b = a == b
    -- al = Arbol izquierdo del objeto a
    -- bl = Arbol izquierdo del objeto b
    Branch a al ar == Branch b bl br = (a == b) && (al == bl) && (ar == br)
    _ == _ = False

arbol1 = Branch 0 (Leaf 1) (Leaf 1)

arbol2 = Branch 1 (Leaf 1) (Leaf 1)

-- >>> show 1
-- "1"


-- root<a<x|y>|b<w|z>>

-- O(n^2)
showTree :: Show a => Tree a -> String
showTree (Leaf x) = show x
showTree (Branch x l r) = show x ++ "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

-- >>>showTree arbol
-- "'*'<'3'|'+'<'1'|'2'>>"

-- O(n)
showsTree :: Show a => Tree a -> String -> String
showsTree (Leaf x) s = shows x s
showsTree (Branch x l r) s = '<' : showsTree l ('|' : showsTree r ('>' : s))

-- >>> showsTree (Leaf '1') ""
-- "'1'"

instance (Show a) => Show (Tree a) where
    show t = showTree t

-- instance (Show a) => Show (Tree a) where
--     showsPrec _ t = showsTree t

-- >>> arbol
-- <'3'|<'1'|'2'>>
