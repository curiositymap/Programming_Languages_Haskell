{- Grammar:

i ∈ Int   ::=   (any integer)
r ∈ Reg   ::=   zero
           |   reg i
e ∈ Exp   ::=   i
           |   r
           |   e + e
c ∈ Cmd   ::=   print e
           |   set r to e
           |   loop e do p end
p ∈ Prog   ::=   ε
           |   c ; p
-}


-- Question 1.

data Reg
   = Zero
   | RegInt Int

data Exp
   = LitInt Int
   | Load Reg
   | Add Exp Exp

data Cmd
   = Print Exp
   | Set Reg Exp
   | Loop Exp Prog

type Prog = [Cmd]

-- Question 2.

ex1 :: Cmd
ex1 = Set (RegInt 2) (Load Zero)

ex2 :: Cmd
ex2 = Loop (LitInt 3) [Print (LitInt 4), Print (LitInt 5)]

ex3 :: Cmd
ex3 = undefined

ex4 :: Cmd
ex4 = Print (Add (LitInt 8) (Load (RegInt 9)))
