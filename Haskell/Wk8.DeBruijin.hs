-- | This module illustrates beta-reduction on nameless lambda calculus
--   terms using de Bruijn indexes.
module DeBruijn where

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)


--
-- * Syntax
--

-- ** Abstract syntax

-- | The de Bruijn index of a variable.
type Var = Int

-- | Nameless lambda calculus terms. Note that we can check alpha-equivalence
--   with plain old Haskell (==). This is a more complicated and expensive
--   operation in named lambda calculuses.
data Exp
   = Ref Var       -- ^ variable reference
   | App Exp Exp   -- ^ application
   | Abs Exp       -- ^ lambda abstraction
  deriving (Eq,Show)


-- ** Syntactic sugar

-- | Build an abstraction that takes two arguments.
abs2 :: Exp -> Exp
abs2 = Abs . Abs

-- | Build an abstraction that takes three arguments.
abs3 :: Exp -> Exp
abs3 = abs2 . Abs

-- | Build an abstraction that takes four arguments.
abs4 :: Exp -> Exp
abs4 = abs3 . Abs

-- | Build an application to apply a function to two arguments.
app2 :: Exp -> Exp -> Exp -> Exp
app2 f e1 e2 = App (App f e1) e2

-- | Build an application to apply a function to three arguments.
app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f e1 e2 e3 = App (app2 f e1 e2) e3

-- | Build an application to apply a function to four arguments.
app4 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
app4 f e1 e2 e3 e4 = App (app3 f e1 e2 e3) e4


-- ** Pretty printing

-- | Pretty print a nameless lambda calculus expression.
pretty :: Exp -> String
pretty e = case e of
    Ref x   -> show x
    Abs e   -> "λ" ++ pretty e
    App l r -> inAppL l ++ " " ++ group r
  where
    group (Ref x) = show x
    group e       = "(" ++ pretty e ++ ")"
    inAppL (App l r) = inAppL l ++ " " ++ group r
    inAppL e         = group e

-- | Print a pretty-printed lambda calculus expression to standard output.
print :: Exp -> IO ()
print = putStrLn . pretty


--
-- * Semantics
--

-- ** Substitution

-- | Variable substitution. `sub x arg e` substitues arg for every x in e.
--
--   Both the abstraction and reference cases are interesting.
--
--   Each time we enter a new abstraction in e, we must:
--    1. Increment the x that we're looking for.
--    2. Increment all of the free variables in arg since now the references
--       will have to skip over one more lambda to get to the lambda they
--       refer to.
--
--   For each variable reference y in e, there are three possibilities:
--     1. It's the variable x we're looking for, in which case we replace it.
--     2. It's a variable bound in e (y < x), in which case we do nothing.
--     3. It's a variable that is free in e (y > x), in which case we
--        decrement it since now the reference has to skip over one less
--        lambda (i.e. the lambda that we beta-reduced away) to get to the
--        lambda it refers to.
--
--   >>> sub 0 (Ref 1) (Abs (Ref 1))
--   Abs (Ref 2)
--
sub :: Var -> Exp -> Exp -> Exp
sub x arg (App l r) = App (sub x arg l) (sub x arg r)
sub x arg (Abs e)  = Abs (sub (x+1) (inc 0 arg) e)
sub x arg (Ref y)
    | y == x = arg        -- found an x, replace it!
    | y < x  = Ref y      -- not x and bound in e, leave it alone
    | y > x  = Ref (y-1)  -- not x and free in e, decrement it

-- | Increment the free variables in an expression.
--
--   The argument d (for "depth") indicates the number of abstractions we
--   have recursed into so far. A variable that is smaller than the depth
--   is not free, and so should not be incremented.
--
--   >>> inc 0 (Ref 0)
--   Ref 1
--
--   >>> inc 0 (App (Ref 1) (Abs (Ref 0)))
--   App (Ref 2) (Abs (Ref 0))
--
inc :: Int -> Exp -> Exp
inc d (App l r) = App (inc d l) (inc d r)
inc d (Abs e)   = Abs (inc (d+1) e)
inc d (Ref x)   = Ref (if x < d then x else x+1)


-- ** Small-step semantics

-- | Do one step of normal order reduction and return the result.
--   If no redex is found, return Nothing.
--
--   The first case matches a redex and does a substitution. The rest of
--   the cases implement a search for next redex.
--
--   >>> step (App (Abs (Ref 0)) (Ref 1))
--   Just (Ref 1)
--
--   >>> step (App (abs2 (App (Ref 0) (Ref 1))) (Ref 2))
--   Just (Abs (App (Ref 0) (Ref 3)))
--
--   >>> step (App (abs2 (App (Ref 2) (Ref 1))) (Ref 0))
--   Just (Abs (App (Ref 1) (Ref 1)))
--
step :: Exp -> Maybe Exp
step (App (Abs e) r) = Just (sub 0 r e)  -- found a redex, do beta reduction!
step (App l r)       = case step l of
                         Just l' -> Just (App l' r)
                         Nothing -> fmap (App l) (step r)
step (Abs e)         = fmap Abs (step e)
step (Ref _)         = Nothing


-- | Evaluate an expression to normal form using normal order recution.
--   Return a list of expressions produced by each step of reduction.
--   Note that this list may be infinite if the reduction never reaches
--   a normal form!
steps :: Exp -> [Exp]
steps e = e : case step e of
                Nothing -> []
                Just e' -> steps e'

-- | Evaluate an expression to normal form using normal order reduction.
--   Note that this function will not terminate if the reduction never
--   reaches a normal form!
eval :: Exp -> Exp
eval = last . steps

-- | Print a reduction sequence for an expression.
printReduce :: Exp -> IO ()
printReduce = mapM_ print . steps


--
-- * Church encodings
--

-- ** Church booleans

-- | λxy.x
true :: Exp
true = abs2 (Ref 1)

-- | λxy.y
false :: Exp
false = abs2 (Ref 0)

-- | λbte.bte
if_ :: Exp
if_ = abs3 (app2 (Ref 2) (Ref 1) (Ref 0))

-- | λb. if b false true
not :: Exp
not = Abs (app3 if_ (Ref 0) false true)

-- | λpq. if p q p
and :: Exp
and = abs2 (app3 if_ (Ref 1) (Ref 0) (Ref 1))

-- | λpq. if p p q
or :: Exp
or = abs2 (app3 if_ (Ref 1) (Ref 1) (Ref 0))

-- | Church Boolean tests:
--
--   >>> true == eval (app2 and (app2 or false true) (App not false))
--   True
--
--   >>> false == eval (app2 or (App not true) (app2 and true false))
--   True


-- ** Church numerals

-- | λfx.x
zero :: Exp
zero = abs2 (Ref 0)

-- | λfx.fx
one :: Exp
one = abs2 (App (Ref 1) (Ref 0))

-- | λfx.f(fx)
two :: Exp
two = abs2 (App (Ref 1) (App (Ref 1) (Ref 0)))

-- | λfx.f(f(fx))
three :: Exp
three = abs2 (App (Ref 1) (App (Ref 1) (App (Ref 1) (Ref 0))))

-- | Build a Church numeral for an arbitrary natural number.
--
--   >>> map num [0,1,2,3] == [zero,one,two,three]
--   True
--
num :: Int -> Exp
num n = abs2 (help n (Ref 0))
  where help 0 e = e
        help n e = App (Ref 1) (help (n-1) e)

-- | λnfx.f(nfx)
succ :: Exp
succ = abs3 (App (Ref 1) (app2 (Ref 2) (Ref 1) (Ref 0)))

-- | λnmfx.nf(mfx)
add :: Exp
add = abs4 (app2 (Ref 3) (Ref 1) (app2 (Ref 2) (Ref 1) (Ref 0)))

-- | λnmf.n(mf)
mult :: Exp
mult = abs3 (App (Ref 2) (App (Ref 1) (Ref 0)))

-- | λn. n (λx.false) true
isZero :: Exp
isZero = Abs (app2 (Ref 0) (Abs false) true)

-- | λnfx.n (λgh.h(gf)) (λu.x) (λu.u)
--
--   See: https://en.wikipedia.org/wiki/Church_encoding#Derivation_of_predecessor_function
pred :: Exp
pred = abs3 (app3 (Ref 2)
                  (abs2 (App (Ref 0) (App (Ref 1) (Ref 3))))
                  (Abs (Ref 1))
                  (Abs (Ref 0)))

-- | Church numeral tests:
--
--   >>> three == eval (app2 add two one)
--   True
--
--   >>> num 15 == eval (app2 mult (app2 add two three) three)
--   True
--
--   >>> num 5 == eval (App pred (num 6))
--   True


-- ** Church tuples

-- | λxys.sxy
--
--   >>> two == eval (App fst (app2 pair two true))
--   True
--
--   >>> true == eval (App snd (app2 pair two true))
--   True
--
pair :: Exp
pair = abs3 (app2 (Ref 0) (Ref 2) (Ref 1))

-- | λt.t(λxy.x)
fst :: Exp
fst = Abs (App (Ref 0) true)

-- | λt.t(λxy.y)
snd :: Exp
snd = Abs (App (Ref 0) false)

-- | λxyzs.sxyz
--
--   >>> one == eval (App sel13 (app3 tuple3 one two three))
--   True
--
--   >>> two == eval (App sel23 (app3 tuple3 one two three))
--   True
--
--   >>> three == eval (App sel33 (app3 tuple3 one two three))
--   True
--
tuple3 :: Exp
tuple3 = abs4 (app3 (Ref 0) (Ref 3) (Ref 2) (Ref 1))

-- | λt.t(λxyz.x)
sel13 :: Exp
sel13 = Abs (App (Ref 0) (abs3 (Ref 2)))

-- | λt.t(λxyz.y)
sel23 :: Exp
sel23 = Abs (App (Ref 0) (abs3 (Ref 1)))

-- | λt.t(λxyz.z)
sel33 :: Exp
sel33 = Abs (App (Ref 0) (abs3 (Ref 0)))


-- ** Church sums

-- | λfgu.ufg
--
--   >>> three == eval (app3 either succ not (App inL two))
--   True
--
--   >>> false == eval (app3 either succ not (App inR true))
--   True
--
either :: Exp
either = pair

-- | λxfg.fx
inL :: Exp
inL = abs3 (App (Ref 1) (Ref 2))

-- | λxfg.gx
inR :: Exp
inR = abs3 (App (Ref 0) (Ref 2))

-- | λfghu.ufgh
--
--   >>> three == eval (app4 case3 succ not fst (App in13 two))
--   True
--
--   >>> false == eval (app4 case3 succ not fst (App in23 true))
--   True
--
--   >>> one == eval (app4 case3 succ not fst (App in33 (app2 pair one two)))
--   True
--
case3 :: Exp
case3 = tuple3

-- | λxfgh.fx
in13 :: Exp
in13 = abs4 (App (Ref 2) (Ref 3))

-- | λxfgh.gx
in23 :: Exp
in23 = abs4 (App (Ref 1) (Ref 3))

-- | λxfgh.hx
in33 :: Exp
in33 = abs4 (App (Ref 0) (Ref 3))


-- ** Fixpoint combinator

-- | λf. (λx.f(xx)) (λx.f(xx))
fix :: Exp
fix = Abs (App (Abs (App (Ref 1) (App (Ref 0) (Ref 0))))
               (Abs (App (Ref 1) (App (Ref 0) (Ref 0)))))

-- | fix (λrn. if (isZero n) one (mult n (r (pred n))))
--
--   >>> num 6 == eval (App fac three)
--   True
--
--   >>> num 24 == eval (App fac (num 4))
--   True
--
fac :: Exp
fac = App fix (abs2
          (app3 if_
                (App isZero (Ref 0))  -- condition
                one                   -- base case
                (app2 mult            -- recursive case
                      (Ref 0)
                      (App (Ref 1) (App pred (Ref 0))))))


-- ** Church-encoded lists

-- | inL (λx.x)
nil :: Exp
nil = App inL (Abs (Ref 0))

-- | λht. inR (pair h t)
cons :: Exp
cons = abs2 (App inR (app2 pair (Ref 1) (Ref 0)))

-- | fix (λrfbl. either (λx.b) (λp. f (fst p) (r f b (snd p))) l)
fold :: Exp
fold = App fix (abs4
           (app3 either
                 (Abs (Ref 2))
                 (Abs (app2 (Ref 3)
                            (App fst (Ref 0))
                            (app3 (Ref 4) (Ref 3) (Ref 2) (App snd (Ref 0)))))
                 (Ref 0)))

-- | Smart constructor to build a Church-encoded list of natural numbers.
list :: [Int] -> Exp
list = foldr (app2 cons . num) nil

-- | fold (λh. add one) zero
--
--   >>> three == eval (App length (list [2,3,4]))
--   True
--
length :: Exp
length = app2 fold (Abs (App add one)) zero

-- | fold add zero
--
--   >>> num 9 == eval (App sum (list [2,3,4]))
--   True
--
--   >>> num 55 == eval (App sum (list [1..10]))
--   True
--
sum :: Exp
sum = app2 fold add zero

-- | fold mult one
--
--   >>> num 24 == eval (App product (list [2,3,4]))
--   True
--
product :: Exp
product = app2 fold mult one

-- | λf. fold (\h. cons (f h)) nil
--
--   >>> eval (list [4,5,6]) == eval (app2 map' (App add two) (list [2,3,4]))
--   True
--
map' :: Exp
map' = Abs (app2 fold (Abs (App cons (App (Ref 1) (Ref 0)))) nil)
