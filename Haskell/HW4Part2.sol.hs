module HW4Part2 where


-- 5. Revised syntax
--
-- reg  ::= `A` | `B` | `R`
--
-- expr ::= int
--       |  reg
--       |  expr + expr
--
-- test ::= expr <= expr
--       |  `not` test
--
-- stmt ::= reg := expr
--       |  `if` test `then` prog `else` prog `end`
--       |  `do` Prog
--       |  `break`


-- 6. Encoded in Haskell

data Reg = A | B | R
  deriving (Eq,Show)

data Expr -- the previous Expr can take both integer and boolean
-- we should divide Expr into two syntactic categories and update the syntax
   = Lit Int
   | Get Reg
   | Add Expr Expr
  deriving (Eq,Show)

data Test
   = LTE Expr Expr -- less than or equal to is applied to integeres and returns boolean
   | Not Test
  deriving (Eq,Show)

data Stmt
   = Set Reg Expr -- for set registration, we expect integer as input, so use exp
   | If Test Prog Prog
   | Do Prog
   | Break
  deriving (Eq,Show)

type Prog = [Stmt]


-- | Programs with type errors... now type errors in Haskell!

-- A := not 3;             { not can only be applied to booleans }
-- B := 4 + (5 <= 6);      { can't add an integer and a boolean }
-- R := 8 <= 7;            { registers may only contain integers }
-- if 2+3 then else end;   { condition must be a boolean }

bad1, bad2, bad3, bad4 :: Prog
bad1 = [Set A (Not (Lit 3))]
bad2 = [Set B (Add (Lit 4) (LTE (Lit 5) (Lit 6)))]
bad3 = [Set R (LTE (Lit 8) (Lit 7))]
bad4 = [If (Add (Lit 2) (Lit 3)) [] []]
