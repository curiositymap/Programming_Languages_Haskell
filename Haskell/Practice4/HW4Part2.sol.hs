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

data Expr
   = Lit Int
   | Get Reg
   | Add Expr Expr
  deriving (Eq,Show)

data Test
   = LTE Expr Expr
   | Not Test
  deriving (Eq,Show)

data Stmt
   = Set Reg Expr
   | If Test Prog Prog
   | Do Prog
   | Break
  deriving (Eq,Show)

type Prog = [Stmt]
