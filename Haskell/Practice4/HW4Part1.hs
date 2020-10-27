{- HW4 

Consider the following abstract syntax, which describes a language that manipulates three integer registers named A, B, and R.

int	::=	(any integer)	integers
 			
reg	::=	A  |  B  |  R	register names
 			
expr	::=	int	           -- integer literal
       |	   reg	           -- load from register
       |	   expr + expr	     -- integer addition
       |	   expr <= expr	  -- less than or equal to
       |	   not expr	        -- boolean negation
 			
stmt	::=	reg := expr	      -- store to register
       |	   if expr	         -- conditional statement
            then prog	
            else prog	
            end	
       |	   do prog	         -- loop until break
            end	
       |	   break	            -- break out of a loop
 			
prog	::=	ε  |  stmt ; prog	-- sequence of statements


Note that although the expression language may produce both booleans and integers, the registers may only contain integers.

Here is an example of a short program in this language that multiplies the numbers in A and B, storing the result in R. Comments, which are not reflected in the abstract syntax, are included in curly braces { … }.

A := 7;           { initialize the registers }
B := 9;
R := 0;
do
  if A <= 0 then  { loop until A is 0 }
    break;
  else
    R := R + B;   { ... add B to R }
    A := A + -1;  { ... decrement A }
  end;
end;

-}

{- Part I -}

-- Question 1. Encode the abstract syntax for the language as a set of Haskell types and data types.

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

data Stmt
   = Store Reg Expr
   | IfElse Expr Prog Prog
   | Do Prog
   | Break
   deriving(Eq,Show)

type Prog = [Stmt]

-- Question 2. Encode the AST of the example program above as a Haskell value.
example :: Prog
example = [Store A (LitInt 7), Store B (LitInt 9), Store R (LitInt 0),
        Do [IfElse (Leq (Load A) (LitInt 0)) [Break] [Store R (Add (Load R) (Load B)), Store A (Add (Load A) (LitInt (-1)))]]]


-- Question 3. Define a function while :: Expr -> Prog -> Stmt that defines a standard while loop as syntactic sugar.
-- You need to know the semantics of the language.

while :: Expr -> Prog -> Stmt
while a b = IfElse (Leq (Load A) (Load B)) [Store R (Leq (Load A) (Load B))] [Break]

-- Question 4. Write a Haskell function sumFromTo :: Int -> Int -> Prog that takes two integers x and y,
-- and returns a program in the object language that sums all of the integers starting from x up to and including y, storing the result in R.
sumFromTo :: Int -> Int -> Prog
sumFromTo x y = [IfElse (Leq (LitInt x) (LitInt y)) (sumFromTo (x+1) y) [Break]]
