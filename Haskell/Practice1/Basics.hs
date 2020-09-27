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
double = undefined

-- | Is this integer zero?
isZero = undefined

-- | Is this integer non-zero?
isNonZero = undefined

-- | Computes the average of two numbers.
avg :: Float -> Float -> Float
avg = undefined

-- | Uses avg to compute half of a number.
half :: Float -> Float
half = undefined


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
safeDiv = undefined

-- | Add two results.
addResults :: Result -> Result -> Result
addResults = undefined

-- | Get the integer from an OK result, or return 0 on an error.
fromResult :: Result -> Int
fromResult = undefined


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
   = Nil
   | Cons Int List
  deriving (Eq,Show)

-- | The empty list.
l0 :: List
l0 = undefined

-- | The list: [1,2,3]
l1 :: List
l1 = undefined

-- | Compute the length of a list.
listLength = undefined

-- | Compute the sum of the integers in a list.
listSum = undefined
