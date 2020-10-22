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


ex2 = [Just True, Nothing]

ex3 = [Leaf one, Leaf two]
-- Type error
{-
data Tree a b
   = Leaf a
   | Node b (Tree a b) (Tree a b)
   deriving (Eq, Show)


ex3 = [Leaf one, Leaf True]

Leaf one --> Leaf Int 1
Leaf True --> Leaf Bool (Type Match Fail)
Therefore, it should be

Tree Int b
-}


ex4 = [Leaf one, Node Nothing (Leaf one) (Leaf two)]

ex5 = Just bit True
-- Type error
{-

ex5 = Just (bit True)

data Maybe a
   = Nothing
   | Just a --> Just takes bit as an argument
   deriving (Eq, Show)

bit :: Bool -> Int

Maybe(Bool -> Int)
-}

ex6 = Just . bit

ex7 = Leaf (Leaf one)

ex8 = bit . even

-- ex9 = even . gt one
-- Type error
{-
even :: Int -> Bool

gt :: Int -> Int -> Bool

The output of gt one is Bool,
Then if you try to put that into even,
it doesn't work since even function expects an Int.
-}

ex10 = gt one . bit

ex11 = Node True (Leaf one) . Leaf

-- ex12 = flip Just
-- Type error
{-
Flip expected: a -> b -> c as an input
Just returned: a -> Maybe a
-}

ex13 = flip map [one,two]

ex14 = treeMap bit even

ex15 = flip treeMap bit even

ex16 = flip (treeMap bit) (Leaf True)
-- Type error
-- bit expects Boolean, but leaf has int


ex17 = flip (treeMap even) (Leaf one)

ex18 = flip (.) even bit


-- ** Create an expression

-- ex19 :: Maybe (Bool -> Int)
-- ex19 = Just bit True

-- ex20 :: Maybe (Maybe a)
-- ex20 = Just (Just 1)

-- ex21 :: Maybe (Maybe Int)
-- ex21 = Just (Just one)

-- ex22 :: Tree Int
-- ex22 = Leaf one
--- why is it impossible?

-- ex23 :: Tree (Maybe a) b
-- ex23 = Leaf nothing
-- why??

-- ex24 :: [Int] -> [Bool]
-- ex24 = map even
-- why ??

-- ex25 :: Tree a Bool -> Tree (Maybe a) Int
-- ex25 = treeMap Just bit

-- ex26 :: Tree Int b -> (b -> d) -> Tree Bool d
-- ex26 = flip (treeMap even)

-- ex27 :: (a -> c) -> Tree a Int -> Tree c Bool
-- ex27 = flip treeMap even
