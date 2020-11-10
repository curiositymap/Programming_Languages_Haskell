module LambdaCalculus where

import Data.Set (Set)
-- Set implementation
import qualified Data.Set as Set
-- Set a name separately because Data.Set functions' names collide with Prelude's functions

-- * Syntax

type Var = String

data Exp
   = Ref Var
   | App Exp Exp
   | Abs Var Exp
   deriving(Eq,Ord,Show)


-- * Syntactic sugar

-- | Define a binary function

abs2 :: Var -> Var -> Exp -> Exp
abs2 x y e = Abs x (Abs y e)

-- | Define a ternary function
abs3 :: Var -> Var -> Var -> Exp -> Exp
abs3 x y z e = Abs x (Abs y (Abs z e))

-- | Apply a binary function
app2 :: Exp -> Exp -> Exp -> Exp
app2 f e1 e2 = App (App f e1) e2

-- | Apply a ternary function
app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f e1 e2 e3 = App (App (App f e1) e2) e3

-- * Define a function that returns free variables * --
-- Input: takes an expression
-- Output: returns set variable


-- How to write this function?
-- PATTERN MATCH!
free :: Exp -> Set Var
free (Ref x)     = Set.singleton x -- the whole expression is just x, so it returns a singleton set
free (App e1 e2) = Set.union (free e1) (free e2) -- we return the union of free variables in both exp1 & exp2
free (Abs x e)   = Set.delete x (free e) -- remove x from the set of free variables in expression

-- We could pattern match
-- But it's easier if we use the implementation of the function defined above, free

closed :: Exp -> Bool
closed e = Set.null . free -- compose two functions
-- closed e = close Set.null (free e)
