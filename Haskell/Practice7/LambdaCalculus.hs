module LambdaCalculus where

import Data.Set (Set)
import qualified Data.Set as Set

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


free :: Exp -> Set Var
free (Ref x)     = Set.singleton x
free (App e1 e2) = Set.union (free e1) (free e2)
free (Abs x e)   = Set.delete x (free e)

closed :: Exp -> Bool
closed e = Set.null . free
