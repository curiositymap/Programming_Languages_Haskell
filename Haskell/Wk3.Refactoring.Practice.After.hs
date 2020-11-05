-- | Refactoring data types: after refactoring
module RefactorDataPost where

-- | Abstract syntax of arithmetic expressions
data Expr
   = Lit Int
   | Bin Op Expr Expr
   deriving (Eq,Show)

-- Arithmetic Operations (Adding specificiy to Bin Op)
data Op = Add | Sub | Mul -- you don't need data types because it's defined above
   deriving (Eq, Show)

-- | Smart constructor for add.
add :: Expr -> Expr -> Expr
add = Bin Add

-- | Smart constructur for sub.
sub :: Expr -> Expr -> Expr
sub = Bin Sub

-- | Smart constructor for mul.
mul :: Expr -> Expr -> Expr
mul = Bin Mul

e1, e2, e3 :: Expr
e1 = sub (mul (Lit 3) (Lit 4)) (Lit 5)
e2 = mul (add (Lit 4) (Lit 5)) (add (Lit 3) (Lit 2))

-- | Let's get the simplified version of leftLit, rightLit, and allLits
-- | Get the leftmost literal in a expression.
leftLit :: Expr -> Int
leftLit (Lit i)   = i
leftLit (Bin _ l _) = leftLit l

-- | Get the rightmost literal in a expression.
rightLit :: Expr -> Int
rightLit (Lit i)   = i
rightLit (Bin _ _ r) = rightLit r

-- | Get a list of all literals in an expression.
allLits :: Expr -> [Int]
allLits (Lit i)   = [i]
allLits (Bin _ l r) = allLits l ++ allLits r

-- | Evaluate an expression.
eval :: Expr -> Int
eval (Lit i)   = i
eval (Bin o l r) = op o (eval l) (eval r)
   where
     op Add = (+)
     op Sub = (-)
     op Mul = (*) -- beautiful abstraction of representing different binary operations


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
