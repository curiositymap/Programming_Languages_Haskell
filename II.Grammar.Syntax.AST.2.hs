{----------------------------------------LEARNING OBJECTIVE
II. Syntax and Naming
1. Given a grammar and program fragment, determine whether the program fragment can be generated from the grammar.
   If so, determine which syntactic category it belongs to.
2. Given a grammar, implement the corresponding abstract syntax as a set of Haskell types and data types.
3. Given an abstract syntax in Haskell, encode a program's AST as a Haskell value.
(4. Determine whether a use of a name is a declaration or reference.)
----------------------------------------------------------}
module HW4Part1 where

{--------------------------------------------------EXAMPLE1
HW4 - Part I. Encoding a grammar in Haskell

Given a grammar describing the concrete syntax of a small PL,
define the abstract syntax as a set of Haskell 'data' and 'type' definitions.
(You can use Haskell's Int type directly.)

<Grammar>
int	::=	(any integer)	 -- integers

reg	::=	A  |  B  |  R	 -- register names

expr	::=	int
       |	reg	          -- load from register
       |	expr + expr
       |	expr <= expr
       |	not expr

stmt	::=	reg := expr	  -- store to register
       |	if expr       -- three arg: expr, prog, prog
          then prog
          else prog
          end
       |	do prog      -- one arg: prog
          end
       |	break        -- no arg

prog	::=	ε
      |  stmt ; prog

----------------------------------------------------------}

-- Each line defined in the grammar requires a data constructor.
-- Pattern match is needed in this case.


data Reg
   = A | B | R   -- A, B, C are all data constructs without argument
   deriving (Eq,Show)

-- You MUST have a data constructor for any argument / each case
-- Each data constructor can take zero or more arguments

data Expr        -- Expr is a type constructor
   = Lit Int     -- Lit is a data constructor, Int is argument
   | Get Reg
   | Add Expr Expr
   | LTE Expr Expr
   | Not Expr
  deriving (Eq,Show)

data Stmt
   = Set Reg Expr
   | If Expr Prog Prog
   | Do Prog
   | Break
  deriving (Eq,Show)

type Prog = [Stmt]


{--------------------------------------------------EXAMPLE1
HW4 - Part II. Encoding Abstract Syntax Tree in Haskell

Determine whether each program below can be genearted by the grammar.
----------------------------------------------------------}

{- Q1: Lines of statements --> Program

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
example :: Prog
example =
  [ Set A (Lit 7)   -- set A register to int 7
  , Set B (Lit 9)   -- set B register to int 9
  , Set R (Lit 0)   -- set C register to int 0
  , Do
    [ If (LTE (Get A) (Lit 0))      -- fst arg of If: Expr
      [ Break ]                     -- snd arg of If: Prog, which is [Stmt]
      [ Set R (Add (Get R) (Get B)) -- trd arg of If: Prog, which is a list of Stmt
      , Set A (Add (Get A) (Lit (-1)))
      ]
    ]
  ]

{- Q2: Use syntactic sugar to write 'While' loop -}

while :: Expr -> Prog -> Stmt
while c b = Do [If c b [Break]]
-- Given a condition c, Expr, and a program b,
-- run it until the loop break and return Stmt.


{- Q3: Program that sums numbers from x to y
(loop in the object language). -}

sumFromTo :: Int -> Int -> Prog
sumFromTo x y =
  [ Set R (Lit 0)   -- Set Register R to 0 (Initialization)
  , Set A (Lit x)   -- Set Register A to x
  , while (LTE (Get A) (Lit y))   -- Conditional statement LessThanEqual to
    [ Set R (Add (Get R) (Get A)) -- When If True, execute the program
    , Set A (Add (Get A) (Lit 1)) -- the program is updating Registers (Reg A, Reg R)
    ]
  ]


{- Q3': Alternative solution
Program that sums numbers from x to y (loop in metalanguage). -}

sumFromTo' :: Int -> Int -> Prog
sumFromTo' x y = Set R (Lit 0) : map add [x..y]
  where add i = Set R (Add (Get R) (Lit i))
