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
-- isZero x = x == 0
isZero 0 = True
isZero _ = False

-- | Is this integer non-zero?
isNonZero :: Int -> Bool
-- isNonZero x = x /= 0
-- isNonZero 0 = False
-- isNonZero _ = True
-- is NonZero x = not (isZero x)
isNonZero = not . isZero

-- | Computes the average of two numbers.
avg :: Float -> Float -> Float
avg x y = (x + y) / 2.0

-- | Uses avg to compute half of a number.
half :: Float -> Float
half x = avg 0 x

(+++) :: Int -> Int -> Int
(+++) x y = 3 * x + 3 * y


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
addResults (OK x) (OK y) = OK (x + y)
addResults _       _     = Error

-- | Get the integer from an OK result, or return 0 on an error.
fromResult :: Result -> Int
fromResult (OK x) = x
fromResult Error = 0


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
-- PATTERN MATCHING
listLength :: List -> Int
listLength Nil     = 0
listLength (Cons h t) = 1 +listLength t -- heads (Int) and tails (List)

-- | Compute the sum of the integers in a list.
listSum :: [Int] -> Int
listSum [] = 0
listSum (h:t) = h + sum t
