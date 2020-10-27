{- Part II -}

-- Question 5. Refactor the syntax of the language to eliminate the possibility of type errors.
-- The new syntax should be able to express all of the type correct programs that could be represented before
-- and none of the type incorrect ones.
-- Write the grammar of the new syntax in a comment in your file.

{-
int	::=	(any integer)	  -- integers

reg	::=	A  |  B  |  R  -- register names

expr	::=	int	          -- integer literal
       |	reg	            -- load from register
       |	expr + expr	    -- integer addition
       |  expr <= expr
       |  not expr

expr' ::= int
       |  bool
       |  reg
       |	expr <= expr	  -- less than or equal to
       |	not expr	      -- boolean negation
       |  expr' + expr'

stmt	::=	reg := expr	   -- store to register
       |  reg := expr'
       |	if expr	       -- conditional statement
          then prog
          else prog
          end
       |  if expr'
          then prog
          else prog
          end
       |	do prog	         -- loop until break
          end
       |	break	           -- break out of a loop

prog	::=	ε  |  stmt ; prog	-- sequence of statements
-}

{- Check the examples below:
A := not 3;             { not can only be applied to booleans } -- make `Reg` accept both Int and Bool?
(Store' A (Neg'(LitInt 3)))

B:= 4 + (5 <= 6);      { can't add an integer and a boolean }  -- make `Add` accept both Int and Bool?
(Store' B (Add' (LitInt' 4) (Leq' (LitInt 5) (LitInt 6))))

R := 8 <= 7;            { registers may only contain integers } -- make `Reg` accept `Expr` as an argument?
if 2+3 then else end;   { condition must be a boolean }         -- make `IfElse` accept `Expr` as an argument?
(Store' R (Leq' (LitInt 8) (LitInt 7)))
-}

-- Expr can be either Int or Bool
-- Split up the Expr cases

-- Question 6. Encode the new abstract syntax as a set of Haskell types and data types.
data Reg
   = A
   | B
   | R
   deriving(Eq,Show)

data Expr
   = LitInt Int
   | Load Reg
   | Add Expr Expr
   | Leq Expr Expr
   | Neg Expr
   deriving(Eq,Show)

data Expr'
   = LitInt' Int
   | LitBool Bool
   | Load' Reg
   | Leq' Expr Expr
   | Neg' Expr
   | Add' Expr' Expr'
   deriving(Eq,Show)

data Stmt
   = Store Reg Expr
   | Store' Reg Expr'
   | IfElse Expr Prog Prog
   | IfElse' Expr' Prog Prog
   | Do Prog
   | Break
   deriving(Eq,Show)

type Prog = [Stmt] -- Prog is a special case. This is a way of writing a list

-- data is used to introduce a new ones
-- type is used to reintroduce things that exist before
