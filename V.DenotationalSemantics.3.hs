{----------------------------------------LEARNING OBJECTIVE
V. Denotation Semantics and Domain Theory
(1. What elements are in a given semantic domain constructed using lifting, sums, and products?)**
2. What is a good choice of semantic domain for a given language?
3. Implement a simple denotational semantics in Haskell.
----------------------------------------------------------}
{--------------------------------------------------EXAMPLE3
Exercise: Denotational Semantics - Calculator Language

<Semantic Domain>
-- Think about all the possible values

    type Reg = Int
    type Domain = Reg -> (Reg, Int)

----------------------------------------------------------}

-- | A single-register calculator language.
module RegCalc where


-- STEP I. Abstract syntax

data Exp
   = Lit Int
   | Add Exp Exp
   | Save Exp
   | Load
  deriving (Eq,Show)

-- save 3 + load  ==>  6
ex1 = Add (Save (Lit 3)) Load

-- save ((save 3 + load) + load) + load  ==>  18
ex2 = Add (Save (Add ex1 Load)) Load


-- STEP II. Define the Denotational Domain

type Reg = Int

type Domain = Reg -> (Reg, Int)

-- STEP III. Write Semantics/valuation function 
sem :: Exp -> Domain
sem (Lit i)   = \s -> (s,i)
sem (Add l r) = \s -> let (sl,i) = sem l s
                          (sr,j) = sem r sl
                      in (sr,i+j)
sem (Save e)  = \s -> let (_,i) = sem e s in (i,i)
sem Load      = \s -> (s,s)
