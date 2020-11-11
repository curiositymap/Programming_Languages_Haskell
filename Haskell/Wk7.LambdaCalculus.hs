module LambdaCalculus where

import Data.Char (isNumber)
import Data.List (find)
import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (and,or,not,pred,succ,fst,snd,either)
import qualified Prelude as Prelude

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


-- ** Naming

-- | Free variables in a lambda calculus term.
--
--   >>> free (abs2 "x" "y" (Ref "x"))
--   fromList []
--
--   >>> free (abs2 "x" "y" (Ref "z"))
--   fromList ["z"]
--
--   >>> free (App (Abs "z" (Ref "z")) (abs2 "x" "y" (Ref "z")))
--   fromList ["z"]
--
--   >>> free (App (Abs "z" (Ref "x")) (abs2 "x" "y" (Ref "z")))
--   fromList ["x","z"]
--

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
closed = Set.null . free -- compose two functions
-- closed e = close Set.null (free e)


-- ** Pretty printing

-- | Pretty print a lambda calculus expression.
pretty :: Exp -> String
pretty e = case e of
    Ref x   -> x
    Abs x e -> "λ" ++ x ++ inAbs e
    App l r -> inAppL l ++ " " ++ group r
  where
    group (Ref x) = x
    group e       = "(" ++ pretty e ++ ")"
    inAbs (Abs x e) = " " ++ x ++ inAbs e
    inAbs e         = ". " ++ pretty e
    inAppL (App l r) = inAppL l ++ " " ++ group r
    inAppL e         = group e


--
-- * Semantics
--

-- ** Substitution

-- | A substitution algorithm.
type Substitution = Var -> Exp -> Exp -> Exp

-- | Naive (unsafe) substitution.
unsafeSub :: Substitution
unsafeSub x arg e@(Ref y) = if x == y then arg else e
unsafeSub x arg (App l r) = App (unsafeSub x arg l) (unsafeSub x arg r)
unsafeSub x arg (Abs y e) = Abs y (unsafeSub x arg e)

-- | Capture-avoiding (safe) substitution. See slide 18.
safeSub :: Substitution
safeSub x arg e@(Ref y) = if x == y then arg else e
safeSub x arg   (App l r) = App (safeSub x arg l) (safeSub x arg r)
safeSub x arg e@(Abs y b) = Abs y' (safeSub x arg (safeSub y (Ref y') b))
  where y' = nextFree y (Set.singleton x `Set.union` free e `Set.union` free arg)

-- | Get the next variable not in the given set of variables.
--   This function assumes that variables are of the form: letter+ number*
--   e.g. foo and foo123 are variables, but 123 is not.
nextFree :: Var -> Set Var -> Var
nextFree x used = fromJust (find (flip Set.notMember used) vars)
  where
    pre = takeWhile (Prelude.not . isNumber) x
    vars = pre : map (\i -> pre ++ show i) [1..]
