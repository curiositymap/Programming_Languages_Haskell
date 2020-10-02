# Practice 1 Code Review 
#### Group 13: Ga Young Lee, Jiangyue Wang, Hongxu Liu, Ashish Vaswani
#### Due: 10/02/2020

##### Code review & a few suggestions to Jiangyue Wang: 
1. Pattern Matching: use the wildcard pattern `_` which matches anything in "leftLit" and "rightLit" functions. By using the underscore, you don't have to hardcode x y and simplify the function to two cases. 
Below is an example of using the wildcard pattern `_` in the "leftLit" function.

``` Haskell
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
leftLit (Mul e _) = leftLit e```
