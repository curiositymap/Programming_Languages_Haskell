{- Notes -}
-- I tried to connenct the concepts and apply Haskell to Church encoding Approach
-- Despite my numerous attempts, I couldn't wrap my head about the assignment.
-- I did my best to synthesize ideas and defined the functions, which is noted in the comment. However, there are some insufficient answers in the assignment.
-- After I get the feedback on the assignment, I will visit the OH to clearly understand the pertinent section.


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

import Data.Set (Set)
import qualified Data.Set as Set

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
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = App ((Ref 0)(Abs (App ((Ref 2) (Ref 1) (Ref 1))))

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
ex3 = app2 (Abs ((abs2(App (Ref 0)(Ref 1)))) (Ref 0)(Ref 1)


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
free :: Exp -> Set Var
free (Ref 0)     = Abs (Ref 0)
free (App (Abs (Ref 0) (Ref 1))) = Set.union (Abs (Ref 0)) (Abs (Ref 1))

-- | Is this lambda calculus term closed?
closed :: Exp -> Bool
closed = Set.null . free

{- Different implementation -}
closed :: Exp -> Bool
closed = check 0
  where
    check d (Ref n)   = n < d -- the reference is inside of the lambda
    check d (Abs e)   = check (d+1) e -- we implement d+1 since there is another abstraction layer
    check d (App l r) = check d l && check d r -- we check both the inside and out


-- d is a depth;
-- reference is smalleres than depth
-- The reason why we get additional when we add an abstraction

0 -- free
 λ0 -- closed
 1    -- free
λ1  -- another layer of abstraction
 λλ1 --


λ 0 1 2 (λ 0 1 2 3) 
λx. x y z (λy. y x w z)  -- y and w should point at the same variable

1 corresponds to y
2 in the second one corresponds to y

what variable name you check is up to you,
but if they're point to the same place, use the same variable 





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
setFst = Abs "t" (App (Ref "t") true)

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
setSnd = Abs "t" (App (Ref "t") false)


--
-- * Part 3: Church encoding a Haskell program
--


We're going to use "Sum"
Use inL and inR

Sum works as Either

Circle -- inL Nat
Rectangle -- inR (Nat, Nat)

λr.   inL r            -- this is Circle
λl w. inR (pair l w) -- this is Rectangle

Let's translate it into a nameless lambda calculus

λ inL 0
λλ inR (pair 1 0) -- l needs to jump over one lambda

Or we could just use λ inR (pair 0 0)


Let's turn them into Haskell w/o Pattern matching

area :: Either Nat (Nat, Nat) -> Nat

However, we're going to use Shape data type.
We can use the data constructors, Circle and Rectangule


area s = either (\r -> pi * r * r) (\p -> fst p * snd p) s
area = either (\r -> pi * r * r) (\p -> fst p * snd p)

Named Lambda Calculus
either (λr. mult (mult 3 r)  r) (λp. mult (fst p) (snd p))

Nameless Lambda Calculus
either (λ mult (mult three 0) 0) (λ mult (fst 0) (snd 0))

Translate this into Haskell encoding -- using Apps and Exp

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
rectangleExp = abs2(app2 pair (Ref 0) (Ref 1))

-- | Convert a shape into a lambda calculus term. This function helps to
--   illustrate how your encodings of the constructors should work.
encodeShape :: Shape -> Exp
encodeShape (Circle r)      = App circleExp (num r)
encodeShape (Rectangle l w) = app2 rectangleExp (num l) (num w)

-- | Encode the square function as a lambda calculus term.
squareExp :: Exp
squareExp = Abs "s" (app2 Abs (Ref 0) (Ref 1))

-- | Encode the area function as a lambda calculus term.
areaExp :: Exp
areaExp = app2 Either(
          (Abs "c" (app3 mult (num 3) (mult (Ref 0) (Ref 1))))
          (Abs "r" (app3 mult (Ref 0) (Ref 1)))
          )
-- Note: I'm not quite sure about the syntax of encoding,
-- but there are two cases (e.g., Circle and Rectangle), so I used Either data type.


-- | Encode the perimeter function as a lambda calculus term.
perimeterExp :: Exp
perimeterExp = app2 Either(
               (Abs "c" (app3 mult (num 3) (Ref 0)))
               (Abs "r" (app3 add (mult (num 2) (Ref 0)) (mult (num 2)(Ref 1))))
               )

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


{- Additional Part -}

-- | Build an abstraction that takes four arguments.
abs5 :: Exp -> Exp
abs5 = abs4 . Abs

-- | λxyzws.sxyzw
tuple4 :: Exp
tuple4 = abs5 "x" "y" "z" "w" "s" (app4 (Ref "s") (Ref "x") (Ref "y") (Ref "z") (Ref "w"))

-- | λt.t(λxyzw.x)
sel14 :: Exp
sel14 = Abs "t" (App (Ref "t") (abs4 "x" "y" "z" "w" (Ref "x")))

-- | λt.t(λxyzw.y)
sel24 :: Exp
sel24 = Abs "t" (App (Ref "t") (abs4 "x" "y" "z" "w"(Ref "y")))

-- | λt.t(λxyzw.z)
sel34 :: Exp
sel34 = Abs "t" (App (Ref "t") (abs4 "x" "y" "z" "w" (Ref "z")))

-- | λt.t(λxyzw.w)
sel44 :: Exp
sel44 = Abs "t" (App (Ref "t") (abs4 "x" "y" "z" "w"(Ref "w")))


-- | λfghiu.ufghi

case4 :: Exp
case4 = tuple4

-- | λxfghi.fx
in14 :: Exp
in14 = abs4 "x" "f" "g" "h" "i" (App (Ref "f") (Ref "x"))

-- | λxfghi.gx
in24 :: Exp
in24 = abs4 "x" "f" "g" "h" "i" (App (Ref "g") (Ref "x"))

-- | λxfghi.hx
in34 :: Exp
in34 = abs4 "x" "f" "g" "h" "i" (App (Ref "h") (Ref "x"))

-- | λxfghi.ix
in44 :: Exp
in44 = abs4 "x" "f" "g" "h" "i" (App (Ref "i") (Ref "x"))
