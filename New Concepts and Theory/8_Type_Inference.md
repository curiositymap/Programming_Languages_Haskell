# Type Inference

### How to determine type classes?

(To be covered in the quiz 1 on 10/20)

e.g., Given e1 and e2, e1 is the function that takes an argument e2.
Therefore, e1 should be T1 (type 1) that takes argument and gives a result.

-- Unification Process:
Goal: check if the type of e2 is equivalent to the type of e1's argument.
Additionally, the type of the result should be equivalent to the outcome of e1e2.

---- This unification step is particularly tricky. However, it's heavily used in type inference.

If there's any of these steps fails, a type error occurs.



Type Inference Exercise:

Q1
Prelude> :t Just
Just :: a -> Maybe a

Q2
Prelude> :t not even 3

<interactive>:1:1: error:
    • Couldn't match expected type ‘t0 -> t’ with actual type ‘Bool’
    • The function ‘not’ is applied to two arguments,
      but its type ‘Bool -> Bool’ has only one
      In the expression: not even 3

<interactive>:1:5: error:
    • Couldn't match expected type ‘Bool’ with actual type ‘a0 -> Bool’
    • Probable cause: ‘even’ is applied to too few arguments
      In the first argument of ‘not’, namely ‘even’
      In the expression: not even 3

Q3
Prelude> :t not (even 3)
not (even 3) :: Bool

Q4
Prelude> :t not . even
not . even :: Integral a => a -> Bool

Q5
Prelude> :t even . not

<interactive>:1:1: error:
    • No instance for (Integral Bool) arising from a use of ‘even’
    • In the first argument of ‘(.)’, namely ‘even’
      In the expression: even . not

Q6
Prelude> :t map (Just . even)
map (Just . even) :: Integral a => [a] -> [Maybe Bool]
