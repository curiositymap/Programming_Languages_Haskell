module TypesPractice where

import Prelude hiding ((.),Maybe(..),even,flip,map)


-- * Notes

-- 1. On the quiz, I'll only give you the types of relevant functions, not
--    their implementations. I assume that you know Haskell's Bool and list
--    data types, and so won't repeat their definitions here or on the quiz.
--    All other data types and relevant function types will be provided.
--
-- 2. For each problem:
--
--    (a) If given an expression, determine whether the expression is type
--        correct. If so, write down its type. If not, write "type error".
--
--    (b) If given a type, determine whether an expression of the given type
--        can be constructed using *only* the definitions in this file. If so,
--        write down the expression. If not, write "impossible".
--
-- 3. All problems are commented out since some are not type correct. You can
--    check an answer by uncommenting an expression and reloading the file
--    in GHCi. If you do not get a type error, you can check the type of the
--    expression with the :type command.
--
-- 4. Many of these problems (especially near the end of the first set) are
--    more difficult than the problems you'll see on the quiz. So if you feel
--    comfortable with all of the problems in this practice, you're in good shape.
--
-- 5. You can pretty easily generate your own problems by just combining these
--    functions and data constructors in various ways.


-- * Definitions

data Maybe a
   = Nothing
   | Just a
  deriving (Eq,Show)

data Tree a b
   = Leaf a
   | Node b (Tree a b) (Tree a b)
  deriving (Eq,Show)

one :: Int
one = 1

two :: Int
two = 2

even :: Int -> Bool
even i = mod i 2 == 0

bit :: Bool -> Int
bit b = if b then 1 else 0

gt :: Int -> Int -> Bool
gt x y = x > y

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
infixr 9 .    -- this says . is right-associative

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

treeMap :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
treeMap f _ (Leaf b)     = Leaf (f b)
treeMap f g (Node a l r) = Node (g a) (treeMap f g l) (treeMap f g r)


-- * Problems


-- ** Determine the type

ex1 :: Tree Int b
ex1 = Leaf one

ex2 :: [Maybe Bool]
ex2 = [Just True, Nothing]

-- type error!
-- ex3 = [Leaf one, Leaf True]

ex4 :: [Tree Int (Maybe a)]
ex4 = [Leaf one, Node Nothing (Leaf one) (Leaf two)]

-- type error!
-- ex5 = Just bit True

ex6 :: Bool -> Maybe Int
       Glob -> Result a
ex6 = Just . bit
      OK . show

ex7 :: Tree (Tree Int b1) b2
ex7 = Leaf (Leaf one)

ex8 :: Int -> Int
ex8 = bit . even

-- type error!
-- ex9 = even . gt one

ex10 :: Bool -> Bool
ex10 = gt one . bit

ex11 :: Int -> Tree Int Bool
ex11 = Node True (Leaf one) . Leaf

-- type error!
-- ex12 = flip Just

ex13 :: (Int -> b) -> [b]
ex13 = flip map [one,two]

ex14 :: Tree Bool Int -> Tree Int Bool
ex14 = treeMap bit even

ex15 :: Tree Int Bool -> Tree Bool Int
ex15 = flip treeMap bit even

-- type error!
-- ex16 = flip (treeMap bit) (Leaf one)

ex17 :: (b -> d) -> Tree Bool d
ex17 = flip (treeMap even) (Leaf one)

ex18 :: Int -> Int
ex18 = flip (.) even bit


-- ** Create an expression

ex19 :: Maybe (Bool -> Int)
ex19 = Just bit

ex20 :: Maybe (Maybe a)
ex20 = Just Nothing

ex21 :: Maybe (Maybe Int)
ex21 = Just (Just one)

-- ex22 :: Tree Int
-- impossible!

ex23 :: Tree (Maybe a) b
ex23 = Leaf Nothing

ex24 :: [Int] -> [Bool]
ex24 = map even

ex25 :: Tree a Bool -> Tree (Maybe a) Int
ex25 = treeMap Just bit

ex26 :: Tree Int b -> (b -> d) -> Tree Bool d
ex26 = flip (treeMap even)

ex27 :: (a -> c) -> Tree a Int -> Tree c Bool
ex27 = flip treeMap even
