{- De Brujin's Conversion -}
{-
\x.\y. x y (\z. x y z)

λλ ? ? (λ ? ? ?)
-- x: need to jump one lambda

λλ 1 ? (λ ? ? ?) -- need to jump over one Lambda
λλ 1 0 (λ ? ? ?) -- no need to jump, so it's zero

another instance of x
λλ 1 0 (λ 2 ? ?) -- since we need to jump over \z and \x
λλ 1 0 (λ 2 1 ?) -- y jumps over \y
λλ 1 0 (λ 2 1 0) -- z jumps over none of the abstraction

-- Is it important to determine the scope of each variable
-- Alpha equivalent expression in the same way

\x.\y. x y (\z. x y z) y
\a.\b. a b (\c. a b c) b

These two can be turned into the same nameless lambda calculus: λλ 1 0 (λ 2 1 0) 0

To check the equivalence, use De Brujin

-}

module HW6 where

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)

import DeBruijn

{-
 (λxy. x) y u
is equivalent to

ex1, ex2 :: Exp
ex1 = app2 (abs2 "x" "y" (Ref "x")) (Ref "y") (Ref "u")
ex2 = Abs "x" (App (abs2 "y" "x" (App (Ref "y") (Ref "x"))) (Ref "x"))

-}
--
-- * Part 1: Nameless lambda calculus
--

-- | λx. (λx.x) x
--
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = App ((Ref x)(Abs (App ((Ref x) x (Ref x))))

-- | (λxy.xz) z z
--
--   >>> eval ex2
--   App (Ref 0) (Ref 0)
--

ex2 :: Exp
ex2 = app2 (abs2 (App (Ref 1)(Ref 2)))(Ref 0)(Ref 0)

-- | λy. (λxy.yx) y z
--
--   >>> eval ex3
--   Abs (App (Ref 1) (Ref 0))
--
ex3 :: Exp
ex3 = app2 (Abs (abs2(App (Ref 0)(Ref 1)))) (Ref 0)(Ref 1)


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
closed = undefined


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

{-
\xy.(pair x (snd y)) 
λλ (pair ? (snd ?))

λλ (pair ? (snd ?)) 
λxys.sxy 


Approach the simplest one --> some redexes in the expression
A pair is a function. It is a shorthand for

:{
--     eval (app2 pair true (num 3)) ==
--     eval (app2 setFst true (app2 pair (num 2) (num 3)))
--   :}

-}
setFst :: Exp
setFst = undefined

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
setSnd = undefined


--
-- * Part 3: Church encoding a Haskell program
--
{- GO over HaskellToChurch file -- is the implementation of the code in Pg 35

inL -- circle
inR -- square

We're using Case 3 / Either

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
circleExp = Abs(Ref 0)

-- | Encode the rectangle constructor as a lambda calculus term. The term
--   should be a function that takes two Church-encoded natural numbers as
--   input and produces a Church-encoded shape as output.
rectangleExp :: Exp
rectangleExp = abs2()

-- | Convert a shape into a lambda calculus term. This function helps to
--   illustrate how your encodings of the constructors should work.
encodeShape :: Shape -> Exp
encodeShape (Circle r)      = App circleExp (num r)
encodeShape (Rectangle l w) = app2 rectangleExp (num l) (num w)

-- | Encode the square function as a lambda calculus term.
squareExp :: Exp
squareExp = undefined

-- | Encode the area function as a lambda calculus term.
areaExp :: Exp
areaExp = undefined

-- | Encode the perimeter function as a lambda calculus term.
perimeterExp :: Exp
perimeterExp = undefined


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
