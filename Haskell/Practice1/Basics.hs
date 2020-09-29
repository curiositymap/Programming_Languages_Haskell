module Basics where


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :help, :load, :reload, :quit, :type, :info
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float, String)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions


---------------------
-- Basic Functions --
---------------------

-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
double :: Int -> Int
double x = x + x

-- | Is this integer zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
-- isZero x = x == 0

-- | Is this integer non-zero?
isNonZero :: Int -> Bool
isNonZero 0 = False
isNonZero _ = True
-- isNonZero x = x /= 0

-- | Computes the average of two numbers.
avg :: Float -> Float -> Float
avg _ 0 = Null
avg x y = (x + y) / 2

-- | Uses avg to compute half of a number.
half :: Float -> Float
half x = avg (x, 0)


-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
--  * anonymous functions


----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name
--   * a set of cases, each with:
--     * a data constructor
--     * zero or more arguments
-- * more pattern matching
--   * top-level and case-expressions

-- | An example data type with two cases.
data Result
   = OK Int
   | Error
  deriving (Eq,Show)

-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv _ 0 = Error
safeDiv x y = OK (x `div` y)

-- | Add two results.
addResults :: Result -> Result -> Result
addResults (OK i) (OK j) = OK (i + j)
addResults _       _     = Error

-- | Get the integer from an OK result, or return 0 on an error.
fromResult :: Result -> Int
-- fromResult (OK i) = i
-- fromResult Error = 0
fromResult r = case r of
                Error -> 0
                OK i -> i


-- The definition of Bool in the Haskell Prelude looks like this:
--
--   data Bool = False | True



---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List
   = Nil -- empty list
   | Cons Int List -- non-empty list
  deriving (Eq,Show)

-- | The empty list.
l0 :: List
l0 = Nil

-- | The list: [1,2,3]
l1 :: List
l1 = Cons 1 (Cons 2 (Cons 3 Nil))

-- | Compute the length of a list.
listLength :: List -> Int
listLength Nil     = 0
listLength (Cons h t) = 1 +listLength t -- heads (Int) and tails (List)

-- | Compute the sum of the integers in a list.
listSum :: [Int] -> Int
listSum [] = 0
listSum (h:t) = h + sum t
