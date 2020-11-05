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
-- In this version, you can use any data type for the list element.

-- This is equivalent to:
--
-- data List a
--     = Nil
--     | Cons a (List a)
-- This version is more abstract than the specified data constructor we've seen in Basics.hs

-- data List
--    = Nil
--    | Cons Int List
--    deriving (Eq, Show)
-- This is the OG list definition we saw before.


-- The definition of String in the Haskell Prelude looks like this:
-- "Hello" is basically equivalent to 'H' : 'e' : 'l' : 'l' : 'o' : []
----  : is basically Cons
-- type String = [Char] -- a list of characters


-- | Compute the length of a list.
length :: [a] -> Int -- you'll be working with arbitrary types -- data type can be anything!
length []     = 0
length (_:t)  = 1 + length t

-- | Compute the sum of an integer list.
sum :: [Int] -> Int -- you need to specify the data type to "Int"
sum []    = 0
sum (h:t) = h + sum t -- the type of (sum t) is Int, so h should be Int too.
-- sum should be able to compatible with h (its input) and (sum t) its output

-- | Compute the product of the elements in an integer list.
-- which means the input will be a list of Int
product :: [Int] -> Int
product []    = 1 -- base case of recursive product function. Therefore, product of an empty list should be 1.
product (h:t) = h * product t -- the type of h is Int, & the type of t is a list of Int

-- | Double all the elements in an integer list.
-- We'll need to return a list of Int
-- Therefore, use Cons (:) to glue two chunks in the RHS to generate a list in the output
doubleAll :: [Int] -> [Int]
doubleAll []    = []
doubleAll (h:t) = 2 * h : doubleAll t

-- | Flip all of the boolean values in a boolean list.
-- Return a list of Bool
-- Thus, use Cons (:) in the RHS to glue chunks and return a list
notAll :: [Bool] -> [Bool]
notAll []    = []
notAll (h:t) = not h : notAll t


----------------------------
-- doubleAll ad notAll are very similiar in the structure.
-- We can factor out the same types and factor them.
-- by using Maps and Folds.
----------------------------

----------------------------
-- Higher-Order Functions --
----------------------------
-- HOF has the ability to use functions as input


-- * Map

-- | Map a function over the elements in a list.

-- The structure repeated in doubleAll and notAll
-- Taking an emtpy list and returning an empty list
-- The other case, (h:t) in LHS and function applied to h and glued to recursive call
-- Since we have a function applied to RHS, it should be applied to LHS
-- Finally, define the type of map
-- Specifically, map takes a function (f) as an input (before the 2nd arrow) that takes an input
-- a list as an input (after the 2nd arrow) and a list as an output (after the 3rd arrow)

map :: (a ->a) -> [a] -> [a]
map f[]     = []
map f(h:t) = f h : map f t


-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (2*) -- same as map(\x ->2*x)

-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' xs = map not xs
-- alternatively, you can just type:
-- notAll' = map not (using Currying)

-- This version of doubleAll and notAll is more elegant and simpler.
-- When you have a pattern that shows up over and over, you can give it a name and take it as input.


-- * Fold
-- This will correspond to the length, sum, and product.

-- | Fold a function over the elements in a list.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x -- the base case
foldr f x (h:t) = f h (foldr f x t) -- f will combine the head and recursive result (Standard Fold Right)

--- Variation Fold Left

foldleft :: (b -> a -> b) -> b -> [a] -> b
foldleft f z []    = z
foldleft f z (h:t) = foldleft f (f z h) t -- named the function 'foldleft' since prelude module contains foldl


-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = foldr (\_ r -> 1 + r) 0 -- anonymous inline function definition
