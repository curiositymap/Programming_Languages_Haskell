-- | Homework 2 template. See the homework description page for more details,
--   hints, and things to think about for each part.
module HW2 where

import HW1


--
-- * Part 1: Reverse Polish Notation
--

-- | Takes an expression and returns a string encoding of that expression in
--   Reverse Polish Notation (RPN).
--
--   >>> toRPN (Lit 3)
--   "3"
--
--   >>> toRPN e1
--   "2 3 4 * +"
--
--   >>> toRPN e2
--   "7 6 + 5 *"
--
--   >>> toRPN e3
--   "3 2 * 5 4 * +"
--
--   >>> elem (toRPN e4) ["8 7 9 * + 6 +", "8 7 9 * 6 + +"]
--   True
--
toRPN :: Expr -> String
toRPN (Lit l) = show l
toRPN (Add l r) = (toRPN l) ++ " " ++ (toRPN r) ++ " +"
toRPN (Mul l r) = (toRPN l) ++ " " ++ (toRPN r) ++ " *"



-- | Takes a string that is an RPN-encoded expression and produces the same
--   expression represented as an abstract syntax tree.
--
--   You can assume that your function will only be given valid RPN-encodings
--   of expressions. That is, it need not fail gracefully if it encounters an
--   error. However, if you would like to improve the error handling, you are
--   welcome to change the type of your function and the doctests.
--
--   >>> fromRPN "3"
--   Lit 3
--
--   >>> fromRPN "2 3 +"
--   Add (Lit 2) (Lit 3)
--
--   >>> fromRPN "2 3 4 + +"
--   Add (Lit 2) (Add (Lit 3) (Lit 4))
--
--   >>> all (\e -> e == fromRPN (toRPN e)) [e1,e2,e3,e4]
--   True
--
fromRPN :: String -> Expr
fromRPN s = head (go [] (words s))
  where
    go :: [Expr] -> [String] -> [Expr]
    go es [] = es
    go (x:y:es) ("+":ss) = go (Add y x : es) ss
    go (x:y:es) ("*":ss) = go (Mul y x : es) ss
    go es       (x:ss)   = go ((Lit (read s)) : es) ss


--
-- * Part 2: Syntactic Sugar
--

-- | Takes an expression and returns an expression that evaluates to its
--   negation. Notice that this function does *not* evaluate the expression!
--   It returns a new expression that, when evaluated, will evaluate to the
--   negation of the original expression.
--
--   >>> eval e2
--   65
--
--   >>> eval (neg e2)
--   -65
--
-- | Map a function over the elements in a list.
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (h:t) = f h : map f t

-- | Reimplement doubleAll using map.
neg :: [Expr] -> [Expr]
neg = map(\x ->"-1"++x)

-- | Takes two expressions and returns an expression that evaluates to the
--   second expression subtracted from the first. Once again, note that the
--   return type is an expression.
--
--   >>> eval e1
--   14
--
--   >>> eval (sub e2 e1)
--   51
--
-- sub :: Expr -> Expr



--- USE FOLDR
