module HW6 where

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)

import DeBruijn

import Data.Set (Set)
import qualified Data.Set as Set

--
-- * Part 1: Nameless lambda calculus
--

-- | λx. (λx.x) x
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = Abs (App (Abs (Ref 0)) (Ref 0))
-- the body of abstraction: the whole thing ((λx.x) x) --> Abs ()
-- Inside the abstraction, there's an app (λx.x) --> App (Abs (Ref 0)) (Ref 0)

-- | (λxy.xz) z z
--
--   >>> eval ex2
--   App (Ref 0) (Ref 0)
--

ex2 :: Exp
ex2 = app2 (abs2 (App (Ref 1) (Ref 2)))(Ref 0)(Ref 0)
-- Application with two arguments: z and z --> app2
-- the body of abstraction has two references: λxy. --> abs2
-- Inside the abstraction, there is an application with xz --> App (Ref 1) (Ref 2)
-- z is free variable outside of the scope of abstraction, so (Ref 0)


-- | λy. (λxy.yx) y z
--
--   >>> eval ex3
--   Abs (App (Ref 1) (Ref 0))
--
ex3 :: Exp
ex3 = Abs (app2 (abs2 (App (Ref 0) (Ref 1)))  (Ref 0) (Ref 1))
-- Abstraction with λy.
-- The body of abstraction is (λxy.yx) y z --> Application with two references: y and z
-- Inside the application, there is an abstraction with two references: λxy.

-- | Is the given nameless lambda calculus term a closed expression? That is,
--   does it contain no free variables?
--
--   >>> closed (Ref 0)
--   False
--
--   >>> closed (Abs (Ref 0))
--   True
--
--   >>> closed (Abs (App (Ref 0) (Ref 1)))
--   False
--
--   >>> closed (Abs (App (Abs (App (Ref 0) (Ref 1))) (Ref 0)))
--   True
--

closed :: Exp -> Bool
closed = check 0
  where
    check d (Ref n)   = n < d -- the reference is inside of the lambda
    check d (Abs e)   = check (d+1) e -- we implement d+1 since there is another abstraction layer
    check d (App l r) = check d l && check d r -- we check both the inside and out


-- d is a depth;
-- reference is smaller than depth
-- The reason why we get additional 1 when we add an abstraction is shown below:
{-
0 -- free
 λ0 -- closed
 1    -- free
λ1  -- another layer of abstraction
 -}



--
-- * Part 2: Church pair update functions
--
{- Professor's OH -}
-- You don't have to worry about the increments / decrements

-- If you want to refer to the first argument, you'd use (Ref 0),
-- If you want to refer to the second argument, you'd use (Ref 1)

{-
abs2 (... (Ref 1) ... (Ref 0) ...)

\x.\y. ... x ... y ...

Ref 1 --> refers to x
Ref 0 --> referes to y

abs2 (... (Abs ... (Ref 2) ...) ...) 
Ref 2 --> refers to x
-}

{-
How to approach constructor / destructor?

Try write a function that takes a pair and changes the first element

-}
-- | Write a lambda calculus function that replaces the first element in a
--   Church-encoded pair. The first argument to the function is the new
--   first element, the second argument is the original pair.
--
--   >>> :{
--     eval (app2 pair true (num 3)) ==
--     eval (app2 setFst true (app2 pair (num 2) (num 3)))
--   :}
--   True
--


setFst :: Exp
setFst = abs2 (app2 pair (Ref 1) (App snd) (Ref 0)))

-- | Write a lambda calculus function that replaces the second element in a
--   Church-encoded pair. The first argument to the function is the new
--   second element, the second argument is the original pair.
--
--   >>> :{
--     eval (app2 pair (num 2) true) ==
--     eval (app2 setSnd true (app2 pair (num 2) (num 3)))
--   :}
--   True
--
setSnd :: Exp
setSnd = abs2 (app2 pair (App fst (Ref 0)) (Ref 1))


--
-- * Part 3: Church encoding a Haskell program
--

{- Prof OH Advice

We're going to use "Sum" with inL and inR

Sum works as Either

STEP 1. Named Lambda Calculus Translation
   Circle -- inL Nat
   Rectangle -- inR (Nat, Nat)

   λr.   inL r            -- this is Circle
   λl w. inR (pair l w)   -- this is Rectangle

STEP 2. Let's translate it into a nameless lambda calculus

   λ inL 0
   λλ inR (pair 1 0) -- l needs to jump over one lambda

   Alternatively, we could just use λ inR (pair 0 0)


STEP 3. Let's turn them into Haskell w/o Pattern matching

   area :: Either Nat (Nat, Nat) -> Nat

   However, we're going to use Shape data type.
   We can use the data constructors, "Circle" and "Rectangle"

   area s = either (\r -> pi * r * r) (\p -> fst p * snd p) s

   Alternatively, we can omit s in both sides.
   area = either (\r -> pi * r * r) (\p -> fst p * snd p)

BONUS:
      1. Named Lambda Calculus
      either (λr. mult (mult 3 r)  r) (λp. mult (fst p) (snd p))

      2. Nameless Lambda Calculus
      either (λ mult (mult three 0) 0) (λ mult (fst 0) (snd 0))

STEP 4. Finally, translate this into Haskell encoding -- using Apps and Exp


* Tips: go over HaskellToChurch file (the implementation of the code in Pg 35).

        inL -- circle
        inR -- square

       In this practice, we're using Case 3 / Either
       
        foo (A n)   = n 
        foo (B b)   = if b then 0 else 1
 foo (C n b) = if b then 0 else n 
        foo v = case v of 
          A n -> ... 
          B b -> ... 
          C n b -> ... 

        λx. f x is equivalent to writing f(x)
        λx. f x <=> f

-}

-- | Pretend Haskell's Int is restricted to Nats.
type Nat = Int

-- | A simple data for representing shapes.
data Shape
   = Circle    Nat
   | Rectangle Nat Nat
  deriving (Eq,Show)

-- | A smart constructor for building squares.
square :: Nat -> Shape
square l = Rectangle l l

-- | Compute the area of a shape using a rough approximation of pi.
area :: Shape -> Nat
area (Circle r)      = 3 * r * r
area (Rectangle l w) = l * w

-- | Compute the perimeter of a shape using a rough approximation of pi.
perimeter :: Shape -> Nat
perimeter (Circle r)      = 2 * 3 * r
perimeter (Rectangle l w) = 2 * l + 2 * w

-- | Encode the circle constructor as a lambda calculus term. The term
--   should be a function that takes a Church-encoded natural number as input
--   and produces a Church-encoded shape as output.
circleExp :: Exp
circleExp = Abs(App InL (Ref 0))

-- | Encode the rectangle constructor as a lambda calculus term. The term
--   should be a function that takes two Church-encoded natural numbers as
--   input and produces a Church-encoded shape as output.
rectangleExp :: Exp
rectangleExp = abs2(App inR (app2 pair (Ref 1) (Ref 0)))

-- | Convert a shape into a lambda calculus term. This function helps to
--   illustrate how your encodings of the constructors should work.
encodeShape :: Shape -> Exp
encodeShape (Circle r)      = App circleExp (num r)
encodeShape (Rectangle l w) = app2 rectangleExp (num l) (num w)

-- | Encode the square function as a lambda calculus term.
squareExp :: Exp
squareExp = Abs (App inR (app2 pair (Ref 0)(Ref 0)))

-- | Encode the area function as a lambda calculus term.
areaExp :: Exp
areaExp = app2 either(
          (Abs "c" (app3 mult (num 3) (mult (Ref 0) (Ref 1))))
          (Abs "r" (app3 mult (Ref 0) (Ref 1)))
          )


-- | Encode the perimeter function as a lambda calculus term.
perimeterExp :: Exp
perimeterExp = app2 either(
               (Abs (app2 mult (app2 mult two three) (Ref 0)))
               (Abs (app2 add (app2 mult two (App fst (Ref 0)))
                              (app2 mult two (App snd (Ref 0))))))


-- | Some tests of your lambda calculus encodings.
--
--   >>> :{
--     checkEval (area (Circle 3))
--       (App areaExp (App circleExp (num 3)))
--   :}
--   True
--
--   >>> :{
--     checkEval (area (Rectangle 3 5))
--       (App areaExp (app2 rectangleExp (num 3) (num 5)))
--   :}
--   True
--
--   >>> :{
--     checkEval (area (square 4))
--       (App areaExp (App squareExp (num 4)))
--   :}
--   True
--
--   >>> :{
--     checkEval (perimeter (Circle 3))
--       (App perimeterExp (App circleExp (num 3)))
--   :}
--   True
--
--   >>> :{
--     checkEval (perimeter (Rectangle 3 5))
--       (App perimeterExp (app2 rectangleExp (num 3) (num 5)))
--   :}
--   True
--
--   >>> :{
--     checkEval (perimeter (square 4))
--       (App perimeterExp (App squareExp (num 4)))
--   :}
--   True
--
checkEval :: Nat -> Exp -> Bool
checkEval n e = num n == eval e
