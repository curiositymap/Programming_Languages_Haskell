module HW1 where


--
-- * A simple arithmetic expression language
--

-- | A representation of arithmetic expressions as binary trees where leaves
--   are literal integers and internal nodes are either addition or
--   multiplaction nodes. Note that this data structure is an "abstract
--   syntax tree", which is something we will spend more time talking about
--   later in the course.

-- In HW1, Expr is like "List" data type excpet that it has three constructors.
data Expr
   = Lit Int         -- ^ Literal integers
   | Add Expr Expr   -- ^ Addition expressions
   | Mul Expr Expr   -- ^ Multiplication expressions
  deriving (Eq,Show)


-- | The expression: 2 + 3 * 4
e1 :: Expr
e1 = Add (Lit 2) (Mul (Lit 3) (Lit 4))


-- | The expression: (7 + 6) * 5
e2 :: Expr
e2 = Mul (Add (Lit 7) (Lit 6)) (Lit 5)


-- | The expresssion: 3 * 2 + 5 * 4
--   Make sure to take standard operator precedence into account.
e3 :: Expr
e3 = Add (Mul (Lit 3) (Lit 2)) (Mul (Lit 5) (Lit 4))


-- | The expression: 8 + 7 * 9 + 6
--   Make sure to take standard operator precedence into account.
e4 :: Expr
e4 = Add (Lit 8) (Add (Mul (Lit 7) (Lit 9)) (Lit 6))


-- | The leftmost literal in an expression.
--
--   >>> leftLit (Lit 3)
--   3
--
--   >>> leftLit e1
--   2
--
--   >>> leftLit e2
--   7
--
leftLit :: Expr -> Int
leftLit (Lit l) = l
leftLit (Add e _) = leftLit e
leftLit (Mul e _) = leftLit e


-- | The rightmost literal in an expression.
--
--   >>> rightLit (Lit 3)
--   3
--
--   >>> rightLit e3
--   4
--
--   >>> rightLit e4
--   6
--
rightLit :: Expr -> Int
rightLit (Lit l) = l
rightLit (Add _ e) = rightLit e
rightLit (Mul _ e) = rightLit e


-- | Get the maximum literal value in an expression.
--
--   >>> maxLit (Lit 3)
--   3
--
--   >>> maxLit e1
--   4
--
--   >>> maxLit e2
--   7
--
--   >>> maxLit e3
--   5
--
--   >>> maxLit e4
--   9
--
maxLit :: Expr -> Int
maxLit (Lit l) = l
maxLit (Add el er) = maxLitBetween el er
maxLit (Mul el er) = maxLitBetween el er

-- Helper function to compare two values
maxLitBetween :: Expr -> Expr -> Int
maxLitBetween el er = (let x = maxLit el in (let y = maxLit er in (if x > y then x else y)))

-- | The integer result of evaluating an expression.
--
--   >>> eval (Lit 3)
--   3
--
--   >>> eval e1
--   14
--
--   >>> eval e2
--   65
--
--   >>> eval e3
--   26
--
--   >>> eval e4
--   77
--
eval :: Expr -> Int
eval (Lit l) = l
eval (Add el er) = (eval el) + (eval er)
eval (Mul el er) = (eval el) * (eval er)


-- | Render an expression as a string (called "pretty printing").
--
--   My solution only adds parentheses when strictly necessary, but it's
--   easier to add them conservatively (that is, to add more than you need).
--   It's OK if your solution adds more parentheses as long as they don't
--   change the meaning of the expression--feel free to tweak the tests to
--   make them pass.
--
--   >>> pretty (Lit 3)
--   "3"
--
--   >>> pretty e1
--   "2 + 3 * 4"
--
--   >>> pretty e2
--   "(7 + 6) * 5"
--
--   >>> pretty e3
--   "3 * 2 + 5 * 4"
--
--   >>> pretty e4
--   "8 + 7 * 9 + 6"
--
--   >>> pretty (Add e1 e2)
--   "2 + 3 * 4 + (7 + 6) * 5"
--
--   >>> pretty (Mul e1 e2)
--   "(2 + 3 * 4) * (7 + 6) * 5"
--
pretty :: Expr -> String
pretty (Lit l) = show l
pretty (Add l r) = "(" ++ (pretty l) ++ " + " ++ (pretty r) ++ ")"
pretty (Mul l r) = (pretty l) ++ " * " ++ (pretty r)
