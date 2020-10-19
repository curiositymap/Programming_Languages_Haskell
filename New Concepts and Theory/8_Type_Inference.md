# Type Inference

### How to determine type classes?

(To be covered in the quiz 1 on 10/21)

e.g., Given e1 and e2, e1 is the function that takes an argument e2.
Therefore, e1 should be T1 (type 1) that takes argument and gives a result.

#### Unification Process:
1. Check if the type of e2 is equivalent to the type of e1's argument.
2. Additionally, the type of the result should be equivalent to the outcome of e1e2.
3. This unification step is particularly tricky. However, it's heavily used in type inference.

If there's any of these steps fails, a type error occurs.


#### | HINT: Find the *top level* application
*Top level* application is the one evaluated *the last*.

Case 1 [O] --> Bottom-up Style

      @
     /  \
    @    3
   / \
not  even

--> not even 3

Case 2 [X]
     @
    / \
  not  @
      / \
    even 3

--> not (even 3)




Type Inference Exercise:

Q1
Prelude> :t Just
Just :: a -> Maybe a

-- "Just" data constructor takes a and returns "Maybe" datatype a

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

--- | HINT: Which one is the *top level* application?
-- *top level* application is the one evaluated *the last*.

      Case 1:

            @
           /  \
          @    3
         / \
      not  even

      --> not even 3

      Case 2:
           @
          / \
        not  @
            / \
          even 3

      --> not (even 3)

--> Unification Fails


Q3
Prelude> :t not (even 3)
not (even 3) :: Bool

LHS: Int -> Bool
RHS: Int ->(?) Int [Match]
Bool -> Bool [Match]

Q4 Function Composition Example
Prelude> :t not . even
--- We can change this into
--- ((.)) not even :t 
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

Q7
Prelude> :t map
map :: (a -> b) -> [a] -> [b]
-- argument is (a -> b)
-- result is [a] -> [b]

Prelude> :t even
even :: Integral a => a -> Bool

Prelude> :t map even
map even :: Integral a => [a] -> [Bool]

Type variable assignment:
