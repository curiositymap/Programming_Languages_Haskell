{--------------------------------------------------EXAMPLE3
Exercise: Denotational Semantics - IMPERATIVE LANGUAGE

STEP I. Define the syntax

<Syntax>

  i ∈ Int   ::=   (any integer)
  r ∈ Reg   ::=   A   |   B
  e ∈ Exp   ::=  i
             |   r
             |   e + e
  s ∈ Stmt   ::=   r ← e
              |   swap
              |   if A < B then s else s
  p ∈ Prog   ::=   s ; p
              |   ε

<Haskell Encoding>
-}

data Reg
   = A | B
   deriving (Eq,Show)

data Expr
   = Lit Int
   | Get Reg
   | Add Expr Expr
   deriving (Eq,Show)

data Test
  = Les Expr Expr

data Stmt
  = Set Reg Expr
  | Swap          -- no argument
  | If Test Stmt Stmt


type Prog = [Stmt]

{- Example Program
A ← 2;
B ← A+3;
if A < B
    then swap
    else B ← A+B;
-}

example :: Prog
example =
  [ Set A (Lit 2),
    Set B (Add (Get A) (Lit 3)),
    If (Les (Get A) (Get B)) (Swap)
       (Set B (Add (Get A) (Get B)))
  ]
  -- <Semantics>
  -- Semantic domains:
  --  * expr: State -> Int
  --  * test: State -> Bool
  --  * stmt: State -> State
  -- | The current values of the registers.
type State = (Int, Int)


-- <Helper Functions>
-- | An initial state, for convenience.
init :: State
init = (0,0)

-- | Get the value of a register.
get :: Reg -> State -> Int
get A (a,_) = a
get B (_,b) = b


-- | Set the value of a register.
set :: Reg -> Int -> State -> State
set A i (_,b) = (i,b)
set B i (a,_) = (a,i)




-- STEP III. Define the valuation function
-- LINK SYNTAX & SEMANTICS -- GIVE MEANING ON SYNTAX
-- | Valuation function for expressions.
expr :: Expr -> State -> Int
expr (Lit i)   = \s -> i
expr (Get r)   = \s -> get r s
expr (Add l r) = \s -> expr l s + expr r s

-- | Valuation function for tests.
test :: Test -> State -> Bool
test (Les l r) = \s -> expr l s <= expr r s

-- | Valuation function for statements.
stmt :: Stmt -> State -> State
stmt (Set r e)   = \s -> set r (expr e s) s
stmt (Swap)      = \s -> set A (expr (Get B)) s
-- stmt (If t s s)  = \s -> if

-- | Helper function for executing a sequence of statements.
stmts :: [Stmt] -> State -> State
stmts []    s = s
stmts (h:t) s = stmts t (stmt h s)
