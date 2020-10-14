-- | Refactoring data types: before refactoring
module RefactorDataPre where

-- Problem statement: there's a redundancy.
-- How to solve? the data type constructor repeated used



-- | Abstract syntax of arithmetic expressions.
data Expr
   = Lit Int         -- ^ Literal integers
   | Add Expr Expr   -- ^ Addition expressions
   | Sub Expr Expr   -- ^ Subtraction expressions
   | Mul Expr Expr   -- ^ Multiplication expressions
  deriving (Eq,Show)


-- | Example expressions.
e1, e2, e3 :: Expr
e1 = Sub (Mul (Lit 3) (Lit 4)) (Lit 5)
e2 = Mul (Add (Lit 4) (Lit 5)) (Add (Lit 3) (Lit 2))
e3 = Sub e2 (Mul (Lit 5) e1)

-- | Get the leftmost literal in a expression.
leftLit :: Expr -> Int
leftLit (Lit i)   = i
leftLit (Add l _) = leftLit l
leftLit (Sub l _) = leftLit l
leftLit (Mul l _) = leftLit l

-- | Get the rightmost literal in a expression.
rightLit :: Expr -> Int
rightLit (Lit i)   = i
rightLit (Add _ r) = rightLit r
rightLit (Sub _ r) = rightLit r
rightLit (Mul _ r) = rightLit r

-- | Get a list of all literals in an expression.
allLits :: Expr -> [Int]
allLits (Lit i)   = [i]
allLits (Add l r) = allLits l ++ allLits r
allLits (Sub l r) = allLits l ++ allLits r
allLits (Mul l r) = allLits l ++ allLits r

-- | Evaluate an expression.
eval :: Expr -> Int
eval (Lit i)   = i
eval (Add l r) = eval l + eval r
eval (Sub l r) = eval l - eval r
eval (Mul l r) = eval l * eval r


-- $ Tests:
--
-- >>> eval e1
-- 7
--
-- >>> eval e2
-- 45
--
-- >>> eval e3
-- 10
