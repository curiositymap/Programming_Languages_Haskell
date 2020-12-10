{----------------------------------------LEARNING OBJECTIVE
IV. Lambda Calculus
1. Determine which variables are free and bound in an expression.
2. Write the reduction sequence for a lambda calculus expression using normal-order or applicative-order reduction.
3. Perform capture-avoiding (safe) substitution.
4. Encode Haskell functions and values as lambda calculus expressions using Church Encoding.
5. Convert to and from nameless lambda calculus using de Bruijin Indices.
----------------------------------------------------------}

{--------------------------------------------------EXAMPLE1
Exercise - De Bruijin Index for Nameless Lambda Calculus

Convert the following lambda calculus expressions into nameless lambda calculus using de Bruijn indices. If a variable is free, assign it the smallest possible index.

1. λx. x (λy. x y)
2. λx. x (λxy. x y)
3. λx. x (λyz. x y)
4. (λx. (λx.x) x) x
5. (λx. (λy.z) z) z
6. λx. (λy. x y) x y
7. λxy. x (x (y x)) y
----------------------------------------------------------}

1. λx. x (λy. x y)
   \   0 ( \  1 0)

2. λx. x (λxy. x y)
    \  0 (\\   1 0)

3. λx. x (λyz. x y)
    \  0 (\\   2 1)

4. (λx. (λx.x) x) x
    \.  ( \ 0) 0) 0 -- the last x is free variable, and has no lambda to jump to reach the imaginary lambda for the x

5. (λx. (λy.z) z) z
    \   (\  2) 1) 0 -- all of the z variables are reaching for the imaginary lambda for z
                    -- especially the free variable, the last z, doesn't need to jump any lambda, so the corresponding index is 0.

6. λx. (λy. x y) x y
   \   (\   1 0) 0 1 -- the last y needs to overcome the scope of \x. so its index is 1

7. λxy. x (x (y x)) y
        1 (1 (0 1)) 0
Convert the following nameless lambda calculus expressions into standard (named) lambda calculus:

8. λλ    0 1 2
   \y.\x.x y z

9.  λ    (λ 1) 0
    \y. (\x.y) y

10. λ (λλ 0 1 2) 0
    \z. (\x.\y. y x z) z

11. λ (λ 0 (1 1)) (λ 0 (1 1))
   \y.(\x.x (y y)) (\z. z (y y))

12. λ 0 (λ 0 (λ 0) (2 1))
   \x.x (\y.y (\z.z)) (w x)

13. λ 0 1 2
    \x. x y z

14. λ 0 1 2   (λ  0 1 2 3)

  \x. x y z  (\w. w x y z)
  \y. y z w  (\x. x y z w)

  {--------------------------------------------------EXAMPLE2
  Exercise - Church Encoding

  <Church booleans>
  true = λx y. x
  false = λx y. y
  if = λb t e. b t e
  and = λp q. if p q p
  or = λp q. if p p q
  not = λp. if p false true

  <Church numerals>
  zero = λf x. x
  one = λf x. f x
  two = λf x. f (f x)
  three = λf x. f (f (f x))
  succ = λn f x. f (n f x)
  add = λn m f x. n f (m f x)
  mult = λn m f. n (m f)
  isZero = λn. n (λx. false) true

  <Church products>
  pair = λx y s. s x y
  fst = λt. t (λx y. x)
  snd = λt. t (λx y. y)
  triple = λx y z s. s x y z
  sel1 = λt. t (λx y z. x)
  sel2 = λt. t (λx y z. y)
  sel3 = λt. t (λx y z. z)

  <Church sums>
  either = λf g u. u f g
  inL = λx f g. f x
  inR = λy f g. g y
  case3 = λf g h u. u f g h
  in1 = λx f g h. f x
  in2 = λy f g h. g y
  in3 = λz f g h. h z

Example: a function that sets the first element of a pair, p, to x
λx p. pair x (snd p)
λx. λp. pair x (snd p)
\x p. pair x (snd p)
\x. \p. pair x (snd p)
----------------------------------------------------------}

type Move = (Nat,Nat)

repeat :: Nat -> Move -> Move
repeat n (dx, dy) = (n * dx, n * dy)

combine :: Move -> Move -> Move
combine (dx1, dy1) (dx2, dy2) = (dx1 + dx2, dy1 + dy2)

data MoveSpec
   = North Nat
   | East Nat
   | Both Nat Nat

toMove :: MoveSpec -> Move
toMove (North dy)   = (0, dy)
toMove (East dx)    = (dx, 0)
toMove (Both dx dy) = (dx, dy)

fromMove :: Move -> MoveSpec
fromMove (0, dy)  = North dy
fromMove (dx, 0)  = East dx
fromMove (dx, dy) = Both dx dy


{----- Church Encoding of the functions below
1. Encode the repeat function in lambda calculus.

<Named Function>
repeat :: Nat -> Move -> Move
repeat n (dx, dy) = (n * dx, n * dy)

<Nameless Function>
repeat = λn m. pair (mult n (fst m)) (mult n (snd m))

-- We need two levels of abstractions: Nat & Move (Coordinate)
-- These two abstractions will be \n.\m.

-- Here, we need both coordinates and multiply n to each coordinate,
         Multiply n to the first coordinate of m,
         (mult n (fst m))
         Multiply n to the second coordinate of m,
         (mult n (snd m))
-- Finally, wrap them around in a pair
            pair (mult n (fst m)) (mult n (snd m))



2. Encode the combine function in lambda calculus.

<Named Function>
combine :: Move -> Move -> Move
combine (dx1, dy1) (dx2, dy2) = (dx1 + dx2, dy1 + dy2)

<Nameless Function>
combine = λm1 m2. pair (add (fst m1) (fst m2)) (add (snd m1) (snd m2))

-- We need two levels of abstractions: Move Coordinate 1 & Move Coordinate 2
-- These two abstractions will be \m1. \m2.

-- Here, we add x1 and x2, (add (fst m1) (fst m2)),
                y1 and y2, (add (snd m1) (snd m2)).
-- The result is another pair,
                pair (add (fst m1) (fst m2)) (add (snd m1) (snd m2)).


3. Encode the toMove function in lambda calculus.
<Named Function>
toMove :: MoveSpec -> Move
toMove (North dy)   = (0, dy)
toMove (East dx)    = (dx, 0)
toMove (Both dx dy) = (dx, dy)

<Nameless Function>
toMove = case3
              (λy. pair zero y)
              (λx. pair x zero)
              (λp. p)

-- There are three cases required for this
-- First, when only moving vertically by y
          (λy. pair zero y)
-- Second, when only moving horizontally by x
          (λx. pair x zero)
-- Lastly, when moving both vertically & horizontally by x and y each
          (λp. p)  --> UPDATE THE PAIR

4. Encode the fromMove function in lambda calculus.

<Named Function>
fromMove :: Move -> MoveSpec
fromMove (0, dy)  = North dy
fromMove (dx, 0)  = East dx
fromMove (dx, dy) = Both dx dy

<Namelss Function>
fromMove = λm. if (isZero (fst m)) (in1 (snd m)) -- update the second coordinate, y, when the first coordinate, x is zero.
                  (if (isZero (snd m))(in2 (fst m))
                                      (in3 m)))


-- We are checking each cases of the move coordinates
-- We need only one abstraction \m.

-- First, check if the first coordinate of the pair, (fst m) is zero
          if so, then just update the second coordinate (snd m) accordingly.
          The result is, (in1 (snd m))
-- If False (when the first coordinate, x, is nonZero) check if the second coordinate is nonZero,
          if so, then update the first coordinate.
          The result is (in2 (fst m))
-- When both If conditions fail, both x and y are nonZero, update the pair as a whole
          The result is (in3 m)
-}
