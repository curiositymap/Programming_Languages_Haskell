{----------------------------------------LEARNING OBJECTIVE
V. Denotation Semantics and Domain Theory
(1. What elements are in a given semantic domain constructed using lifting, sums, and products?)**
2. What is a good choice of semantic domain for a given language?
3. Implement a simple denotational semantics in Haskell.
----------------------------------------------------------}

{--------------------------------------------------EXAMPLE2
Exercise: Denotational Semantics - IntBool Language

<Semantic Domain>
-- Think about all the possible values

    data Value
       = I Int
       | B Bool
       | Error
      deriving (Eq,Show)
----------------------------------------------------------}

-- | A simple expression language with two types.
module IntBool where

import Prelude hiding (not,and,or)


-- 1. Define the abstract syntax as a Haskell data type.

data Exp
   = LitI Int
   | LitB Bool
   | Neg Exp
   | Add Exp Exp
   | Mul Exp Exp
   | Equ Exp Exp
   | If  Exp Exp Exp
  deriving (Eq,Show)

-- Some example expressions:

-- | 2 * (3 + 4)  ==>  14
ex1 :: Exp
ex1 = Mul (LitI 2) (Add (LitI 3) (LitI 4))

-- | 2 * (3 + 4) == 10  ==>  false
ex2 :: Exp
ex2 = Equ ex1 (LitI 10)

-- | 2 * (3 + 4) ? 5 : 6  ==>  type error!
ex3 :: Exp
ex3 = If ex1 (LitI 5) (LitI 6)

-- | 2 * (3 + 4) == 10 ? 5 : 6  ==>  6
ex4 :: Exp
ex4 = If ex2 (LitI 5) (LitI 6)


-- STEP II. Identify/ the semantic domain for this language
--
--   * what types of values can we have?
--     * Int
--     * Bool
--     * Error
--
--   * how can we express this in Haskell?

      data Value
         = I Int
         | B Bool
         | Error
        deriving (Eq,Show)

-- Alternative semantics domain using Maybe and Either:
--
--   data Maybe a = Nothing | Just a
--   data Either a b = Left a | Right b
--
--   type Value = Maybe (Either Int Bool)
--
-- Example semantic values in both representations:
--
--   I 6     <=>  Just (Left 6)
--   B True  <=>  Just (Right True)
--   Error   <=>  Nothing
--
-- Which of the following are isomorphic to Value?
--
--   type Value1 = Either (Maybe Int) Bool  -- yes
--     I 6     <=>  Left (Just 6)
--     B True  <=>  Right True
--     Error   <=>  Left Nothing
--   type Value2 = Either Int (Maybe Bool)  -- yes
--     I 6     <=>  Left 6
--     B True  <=>  Right (Just True)
--     Error   <=>  Right Nothing
--   type Value3 = Either (Maybe Int) (Maybe Bool)  -- no!
--     I 6     <=>  Left (Just 6)
--     B True  <=>  Right (Just True)
--     Error   <=>  Left Nothing --or-- Right Nothing ??
--


-- STEP III. Define the valuation function
-- LINK SYNTAX & SEMANTICS -- GIVE MEANING ON SYNTAX

sem :: Exp -> Value
sem (LitI i)   = I i
sem (LitB b)   = B b
sem (Neg e)    = case sem e of
                   I i -> I (negate i)
                   _   -> Error
sem (Add l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i + j)
                   _          -> Error
sem (Mul l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i * j)
                   _          -> Error
sem (Equ l r)  = case (sem l, sem r) of
                   (I i, I j) -> B (i == j)
                   (B a, B b) -> B (a == b)
                   _          -> Error
sem (If c t e) = case sem c of
                   B True  -> sem t
                   B False -> sem e
                   _       -> Error
