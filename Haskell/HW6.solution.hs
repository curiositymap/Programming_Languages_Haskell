module HW6 where

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)

import DeBruijn


--
-- * Part 1: Nameless lambda calculus
--

-- | λx. (λx.x) x
--
--   >>> eval ex1
--   Abs (Ref 0)
--
ex1 :: Exp
ex1 = Abs (App (Abs (Ref 0)) (Ref 0))

-- | (λxy.xz) z z
--
--   >>> eval ex2
--   App (Ref 0) (Ref 0)
--
ex2 :: Exp
ex2 = app2 (abs2 (App (Ref 1) (Ref 2))) (Ref 0) (Ref 0)

-- | λy. (λxy.yx) y z
--
--   >>> eval ex3
--   Abs (App (Ref 1) (Ref 0))
--
ex3 :: Exp
ex3 = Abs (app2 (abs2 (App (Ref 0) (Ref 1))) (Ref 0) (Ref 1))


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
    check d (Ref n)   = n < d
    check d (Abs e)   = check (d+1) e
    check d (App l r) = check d l && check d r


--
-- * Part 2: Church pair update functions
--

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
setFst = abs2 (app2 pair (Ref 1) (App snd (Ref 0)))

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
circleExp = Abs (App inL (Ref 0))

-- | Encode the rectangle constructor as a lambda calculus term. The term
--   should be a function that takes two Church-encoded natural numbers as
--   input and produces a Church-encoded shape as output.
rectangleExp :: Exp
rectangleExp = abs2 (App inR (app2 pair (Ref 1) (Ref 0)))

-- | Convert a shape into a lambda calculus term. This function helps to
--   illustrate how your encodings of the constructors should work.
encodeShape :: Shape -> Exp
encodeShape (Circle r)      = App circleExp (num r)
encodeShape (Rectangle l w) = app2 rectangleExp (num l) (num w)

-- | Encode the square function as a lambda calculus term.
squareExp :: Exp
squareExp = Abs (App inR (app2 pair (Ref 0) (Ref 0)))

-- | Encode the area function as a lambda calculus term.
areaExp :: Exp
areaExp = app2 either
    (Abs (app2 mult (app2 mult three (Ref 0)) (Ref 0)))
    (Abs (app2 mult (App fst (Ref 0)) (App snd (Ref 0))))

-- | Encode the perimeter function as a lambda calculus term.
perimeterExp :: Exp
perimeterExp = app2 either
    (Abs (app2 mult (app2 mult two three) (Ref 0)))
    (Abs (app2 add (app2 mult two (App fst (Ref 0)))
                   (app2 mult two (App snd (Ref 0)))))


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
