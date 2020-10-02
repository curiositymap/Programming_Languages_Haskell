module Lists where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [] a
--      = [] -- base case
--      | (:) a [a] -- recursive call assuming that your function works!

-- This is equivalent to:
--
-- data List a
--     = Nil
--     | Cons a (List a)

-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
length [] = 0
length (_:t) = 1 + length t

-- | Compute the sum of an integer list.
sum :: [Int] -> Int
sum [] = 0
sum (h:t) = h + sum t

-- | Compute the product of the elements in an integer list.
product :: [Int] -> Int
product [] = 1 -- base case of recursive product function. Therefore, product of an empty list should be 1.
product (h:t) = h * product t

-- | Double all the elements in an integer list.
doubleAll :: [Int] -> [Int]
doubleAll [] = []
doubleAll (h:t) = 2 * h : doubleAll t

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll [] = []
notAll (h:t) = not h : notAll t


----------------------------
-- Higher-Order Functions --
----------------------------
-- HOF has the ability to use functions as input


-- * Map

-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (h:t) = f h : map f t

-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (2*) -- (\x ->2*x)

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not

-- This version of doubleAll and notAll is more elegant and simpler.
-- When you have a pattern that shows up over and over, you can give it a name and take it as input.


-- * Fold
-- This will correspond to the length, sum, and product.

-- | Fold a function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x -- the base case
foldr f x (h:t) = f h (foldr f x t) -- f will combine the head and recursive result


-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = foldr (\_ r -> 1 + r) 0 -- anonymous inline function definition
