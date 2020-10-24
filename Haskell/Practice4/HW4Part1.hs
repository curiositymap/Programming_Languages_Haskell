-- Haskell Data Type Definitions & Haskell Types

data Term = Not Term
          | If Term Term Term
          | Lit Bool

-- Haskell Data Value Examples

ex1 = Lit True
ex2 = If (Lit True) (Lit True) (Lit False)
ex3 = Not(Not(Lit False))

-- Haskell encoding

data Num = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
data Int = Single Num | Multiple Num Int
data Reg = A | B | R

data Exp = ExpInt Int
         | ExpReg Reg
         | ExpAdd Exp Exp
         | ExpLeq Exp Exp
         | ExpNeg Exp
