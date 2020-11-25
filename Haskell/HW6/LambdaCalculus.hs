{-
import Data.Set (Set)
import qualified Data.Set as Set

These two lines above should be manually typed & loaded at the beginning of running GHCI
for some reason (maybe due to the ghci compatibility issues).

-}
module LambdaCalculus where

import Data.Char (isNumber)
import Data.List (find)
import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)
import qualified Prelude as Prelude


--
-- * Syntax
--

-- ** Abstract syntax

-- | Variable names.
type Var = String

-- | Lambda calculus terms.
data Exp
   = Ref Var
   | App Exp Exp
   | Abs Var Exp
  deriving (Eq,Ord,Show)


-- ** Syntactic sugar

-- | Build an abstraction that takes two arguments.
-- Takes two variables as arguments
-- and the body of abstraction as expression
-- to produce a new expression that encodes the binary function

abs2 :: Var -> Var -> Exp -> Exp
abs2 x1 x2 e = Abs x1 (Abs x2 e)

-- | Build an abstraction that takes three arguments.
abs3 :: Var -> Var -> Var -> Exp -> Exp
abs3 x1 x2 x3 e = Abs x1 (abs2 x2 x3 e)

-- | Build an abstraction that takes four arguments.
abs4 :: Var -> Var -> Var -> Var -> Exp -> Exp
abs4 x1 x2 x3 x4 e = Abs x1 (abs3 x2 x3 x4 e)

-- | Build an application to apply a function to two arguments.
-- First Exp is the function we're applying
-- Second Exp is the argument
-- Third Exp is the second argument
-- Last Exp is the new Exp we're returning

app2 :: Exp -> Exp -> Exp -> Exp
app2 f e1 e2 = App (App f e1) e2

-- | Build an application to apply a function to three arguments.
app3 :: Exp -> Exp -> Exp -> Exp -> Exp
app3 f e1 e2 e3 = App (app2 f e1 e2) e3

-- | Build an application to apply a function to four arguments.
app4 :: Exp -> Exp -> Exp -> Exp -> Exp -> Exp
app4 f e1 e2 e3 e4 = App (app3 f e1 e2 e3) e4


-- ** Naming

-- | Free variables in a lambda calculus term.
--
--   >>> free (abs2 "x" "y" (Ref "x"))
--   fromList []
--
--   >>> free (abs2 "x" "y" (Ref "z"))
--   fromList ["z"]
--
--   >>> free (App (Abs "z" (Ref "z")) (abs2 "x" "y" (Ref "z")))
--   fromList ["z"]
--
--   >>> free (App (Abs "z" (Ref "x")) (abs2 "x" "y" (Ref "z")))
--   fromList ["x","z"]
--
free :: Exp -> Set Var
free (Ref x)     = Set.singleton x
free (App e1 e2) = Set.union (free e1) (free e2)
free (Abs x e)   = Set.delete x (free e)

-- | Is this lambda calculus term closed?
closed :: Exp -> Bool
closed = Set.null . free

-- | Are these expressions alpha-equivalent? That is, can we rename the
--   variables in one to make them syntactically identical?
equiv :: Exp -> Exp -> Bool
equiv (Ref x1)    (Ref x2)    = x1 == x2
equiv (App l1 r1) (App l2 r2) = equiv l1 l2 && equiv r1 r2
equiv (Abs x1 e1) (Abs x2 e2)
    | x1 == x2  = equiv e1 e2
    | otherwise = equiv e1 (rename x1 x2 e2)
  where
    rename new old e@(Ref x)
        | x == old  = Ref new
        | otherwise = e
    rename new old (Abs x e)
        | x == old  = Abs x e
        | x == new  = let x' = nextFree x (Set.singleton x `Set.union` free e)
                      in Abs x' (rename new old (rename x' x e))
        | otherwise = Abs x (rename new old e)
    rename new old (App l r) = App (rename new old l) (rename new old r)


-- ** Pretty printing

-- | Pretty print a lambda calculus expression.
pretty :: Exp -> String
pretty e = case e of
    Ref x   -> x
    Abs x e -> "λ" ++ x ++ inAbs e
    App l r -> inAppL l ++ " " ++ group r
  where
    group (Ref x) = x
    group e       = "(" ++ pretty e ++ ")"
    inAbs (Abs x e) = " " ++ x ++ inAbs e
    inAbs e         = ". " ++ pretty e
    inAppL (App l r) = inAppL l ++ " " ++ group r
    inAppL e         = group e

-- | Print a pretty-printed lambda calculus expression to standard output.
print :: Exp -> IO ()
print = putStrLn . pretty


--
-- * Semantics
--

-- ** Substitution

-- | A substitution algorithm.
type Substitution = Var -> Exp -> Exp -> Exp

-- | Naive (unsafe) substitution.
unsafeSub :: Substitution
unsafeSub x arg e@(Ref y) = if x == y then arg else e
unsafeSub x arg (App l r) = App (unsafeSub x arg l) (unsafeSub x arg r)
unsafeSub x arg (Abs y e) = Abs y (unsafeSub x arg e)

-- | Capture-avoiding (safe) substitution. See slide 18.
safeSub :: Substitution
safeSub x arg e@(Ref y) = if x == y then arg else e
safeSub x arg   (App l r) = App (safeSub x arg l) (safeSub x arg r)
-- Rename y to y' and conduct the two substitutions
-- We got a helper function "nextFree" to figure out the name of free variable we're substituting
safeSub x arg e@(Abs y b) = Abs y' (safeSub x arg (safeSub y (Ref y') b))
  where y' = nextFree y (Set.singleton x `Set.union` free e `Set.union` free arg)

-- | Get the next variable not in the given set of variables.
--   This function assumes that variables are of the form: letter+ number*
--   e.g. foo and foo123 are variables, but 123 is not.
nextFree :: Var -> Set Var -> Var
nextFree x used = fromJust (find (flip Set.notMember used) vars)
  where
    pre = takeWhile (Prelude.not . isNumber) x
    vars = pre : map (\i -> pre ++ show i) [1..]


-- ** Small-step semantics

step :: Substitution -> Exp -> Maybe Exp
step sub (App (Abs x e) r) = Just (sub x r e)
step sub (App l r)         = case step sub l of
                              Just l' -> Just (App l' r)
                              Nothing -> fmap (App l) (step sub r)
step sub (Abs x e)         = fmap (Abs x) (step sub e)
step _   (Ref _)           = Nothing

steps :: Substitution -> Exp -> [Exp]
steps sub e = e : case step sub e of
                   Nothing -> []
                   Just e' -> steps sub e'


-- | A single-step reduction parameterized by a substitution algorithm.
-- type Step = Substitution -> Exp -> Maybe Exp

-- | Do one step of normal-order reduction using the given substitution
--   algorithm and return the result. If no redex is found, return Nothing.
--
--   The first case matches a redex and does a substitution. The rest of
--   the cases implement a search for next redex.
-- stepN :: Step
-- stepN sub (App (Abs x e) r) = Just (sub x r e)
-- stepN sub (App l r)         = case stepN sub l of
--                                 Just l' -> Just (App l' r)
--                                 Nothing -> fmap (App l) (stepN sub r)
-- stepN sub (Abs x e)         = fmap (Abs x) (stepN sub e)
-- stepN _   (Ref _)           = Nothing
--
-- -- | Do one step of applicative-order reduction using the given substitution
-- --   algorithm and return the result. If no redex is found, return Nothing.
-- stepA :: Step
-- stepA sub (App (Abs x e) r)
--     | Just e' <- stepA sub e = Just (App (Abs x e') r)
--     | Just r' <- stepA sub r = Just (App (Abs x e) r')
--     | otherwise              = Just (sub x r e)
-- stepA sub (App l r)
--     | Just l' <- stepA sub l = Just (App l' r)
--     | otherwise              = fmap (App l) (stepA sub r)
-- stepA sub (Abs x e) = fmap (Abs x) (stepA sub e)
-- stepA _   (Ref _)   = Nothing

-- | Evaluate an expression to normal form using the given substitution
--   algorithm and a step function. Return a list of expressions produced
--   by each step of reduction. This list may be infinite if the expression
--   does not reduce to normal form using the given step function.
-- steps :: Substitution -> Step -> Exp -> [Exp]
-- steps sub step e = e : case step sub e of
--                          Nothing -> []
--                          Just e' -> steps sub step e'

-- | Evaluate an expression to normal form using safe substitution and
--   normal-order reduction. Note that this function will not terminate
--   if the reduction never reaches a normal form!
eval :: Substitution -> Exp -> Exp
eval sub = last . steps sub

-- | Evaluate an expression to normal form using unsafe substitution and
--   normal-order reduction. Note that this function will not terminate
--   if the reduction never reaches a normal form!
unsafeEval :: Exp -> Exp
unsafeEval = eval unsafeSub

safeEval :: Exp -> Exp
safeEval = eval safeSub

-- | Print a reduction sequence for an expression using safe substitution
--   and the given step function.
printReduce :: Exp -> IO ()
printReduce = mapM_ (putStrLn. pretty) . steps safeSub
-- | Print a normal-order reduction sequence for an expression.
-- printReduceN :: Exp -> IO ()
-- printReduceN = printReduce stepN
--
-- -- | Print an applicative-order reduction sequence for an expression.
-- printReduceA :: Exp -> IO ()
-- printReduceA = printReduce stepA


-- ** Examples and tests

-- | Variable capture examples from slide 17.
-- | (λxy. x) y u

ex1, ex2 :: Exp
ex1 = app2 (abs2 "x" "y" (Ref "x")) (Ref "y") (Ref "u")
ex2 = Abs "x" (App (abs2 "y" "x" (App (Ref "y") (Ref "x"))) (Ref "x"))

-- | Test the variable capture examples.
--
--   >> unsafeEval ex1
--   Ref "u"
--
--   >> eval ex1
--   Ref "y"
--
--   >> unsafeEval ex2
--   Abs "x" (Abs "x" (App (Ref "x") (Ref "x")))
--
--   >> eval ex2
--   Abs "x" (Abs "x1" (App (Ref "x") (Ref "x1")))

-- | Reduction examples from slide 25.
ex3, ex4 :: Exp
ex3 = App (Abs "x" (App (Ref "x") (Ref "x")))
          (app2 (abs2 "x" "y" (App (Ref "y") (Ref "x")))
                (Ref "z")
                (Abs "x" (Ref "x")))
ex4 = app3 (abs3 "x" "y" "z" (App (Ref "x") (Ref "z")))
           (Abs "z" (Ref "z"))
           (App (Abs "y" (Ref "y")) (Abs "z" (Ref "z")))
           (Ref "x")


--
-- * Church encodings
--

-- ** Church booleans

-- | λxy.x
true :: Exp
true = abs2 "x" "y" (Ref "x")

-- | λxy.y
false :: Exp
false = abs2 "x" "y" (Ref "y")

-- | λbte.bte
if_ :: Exp
if_ = abs3 "b" "t" "e" (app2 (Ref "b") (Ref "t") (Ref "e"))

-- | λb. if b false true
not :: Exp
not = Abs "b" (app3 if_ (Ref "b") false true)

-- | λpq. if p q p
and :: Exp
and = abs2 "p" "q" (app3 if_ (Ref "p") (Ref "q") (Ref "p"))

-- | λpq. if p p q
or :: Exp
or = abs2 "p" "q" (app3 if_ (Ref "p") (Ref "p") (Ref "q"))


-- | Church Boolean tests. One of these currently fails. Why and how can we fix it?
--
--   >>> true `equiv` eval (app2 and (app2 or false true) (App not false))
--   True
--
--   >>> false `equiv` eval (app2 or (App not true) (app2 and true false))
--   True


-- ** Church numerals

-- | λfx.x -- apply f to x 0 time
zero :: Exp
zero = abs2 "f" "x" (Ref "x")

-- | λfx.fx -- apply f to x 1 time
one :: Exp
one = abs2 "f" "x" (App (Ref "f") (Ref "x"))

-- | λfx.f(fx)
two :: Exp
two = abs2 "f" "x" (App (Ref "f") (App (Ref "f") (Ref "x")))

-- | λfx.f(f(fx))
three :: Exp
three = abs2 "f" "x" (App (Ref "f") (App (Ref "f") (App (Ref "f") (Ref "x"))))

-- | Build a Church numeral for an arbitrary natural number.
--
--   >>> map num [0,1,2,3] == [zero,one,two,three]
--   True
--
num :: Int -> Exp
num n = abs2 "f" "x" (help n (Ref "x"))
  where help 0 e = e
        help n e = App (Ref "f") (help (n-1) e)

-- | λnfx.f(nfx) -- apply f to x n times and apply f one time afterwards
succ :: Exp
succ = abs3 "n" "f" "x" (App (Ref "f") (app2 (Ref "n") (Ref "f") (Ref "x")))

-- | λnmfx.nf(mfx)
-- take arguments, n and m,
-- apply f to x (m) times and
-- apply f to the result n times
add :: Exp
add = abs4 "n" "m" "f" "x" (app2 (Ref "n") (Ref "f") (app2 (Ref "m") (Ref "f") (Ref "x")))

-- | λnmf.n(mf)
-- apply m applications of f on x
-- apply n applications of (mfx)
mult :: Exp
mult = abs3 "n" "m" "f" (App (Ref "n") (App (Ref "m") (Ref "f")))

-- | λn. n (λx.false) true
-- 
isZero :: Exp
isZero = Abs "n" (app2 (Ref "n") (Abs "x" false) true)

-- | λnfx.n (λgh.h(gf)) (λu.x) (λu.u)
--
--   See: https://en.wikipedia.org/wiki/Church_encoding#Derivation_of_predecessor_function
pred :: Exp
pred = abs3 "n" "f" "x"
            (app3 (Ref "n")
                  (abs2 "g" "h" (App (Ref "h") (App (Ref "g") (Ref "f"))))
                  (Abs "u" (Ref "x"))
                  (Abs "u" (Ref "u")))


-- | Church numeral tests:
--
--   >>> three `equiv` eval (app2 add two one)
--   True
--
--   >>> num 15 `equiv` eval (app2 mult (app2 add two three) three)
--   True
--
--   >>> num 5 `equiv` eval (App pred (num 6))
--   True


-- ** Church tuples

-- | λxys.sxy
--
--   >>> two `equiv` eval (App fst (app2 pair two true))
--   True
--
--   >>> true `equiv` eval (App snd (app2 pair two true))
--   True
--
pair :: Exp
pair = abs3 "x" "y" "s" (app2 (Ref "s") (Ref "x") (Ref "y"))

-- | λt.t(λxy.x)
fst :: Exp
fst = Abs "t" (App (Ref "t") true)

-- | λt.t(λxy.y)
snd :: Exp
snd = Abs "t" (App (Ref "t") false)


-- | λxyzs.sxyz
--
--   >>> one `equiv` eval (App sel13 (app3 tuple3 one two three))
--   True
--
--   >>> two `equiv` eval (App sel23 (app3 tuple3 one two three))
--   True
--
--   >>> three `equiv` eval (App sel33 (app3 tuple3 one two three))
--   True
--
tuple3 :: Exp
tuple3 = abs4 "x" "y" "z" "s" (app3 (Ref "s") (Ref "x") (Ref "y") (Ref "z"))

-- | λt.t(λxyz.x)
sel13 :: Exp
sel13 = Abs "t" (App (Ref "t") (abs3 "x" "y" "z" (Ref "x")))

-- | λt.t(λxyz.y)
sel23 :: Exp
sel23 = Abs "t" (App (Ref "t") (abs3 "x" "y" "z" (Ref "y")))

-- | λt.t(λxyz.z)
sel33 :: Exp
sel33 = Abs "t" (App (Ref "t") (abs3 "x" "y" "z" (Ref "z")))


-- ** Church sums

-- | λfgu.ufg
--
--   >>> three `equiv` eval (app3 either succ not (App inL two))
--   True
--
--   >>> false `equiv` eval (app3 either succ not (App inR true))
--   True
--
either :: Exp
either = pair

-- | λxfg.fx
inL :: Exp
inL = abs3 "x" "f" "g" (App (Ref "f") (Ref "x"))

-- | λxfg.gx
inR :: Exp
inR = abs3 "x" "f" "g" (App (Ref "g") (Ref "x"))


-- | λfghu.ufgh
--
--   >>> three `equiv` eval (app4 case3 succ not fst (App in13 two))
--   True
--
--   >>> false `equiv` eval (app4 case3 succ not fst (App in23 true))
--   True
--
--   >>> one `equiv` eval (app4 case3 succ not fst (App in33 (app2 pair one two)))
--   True
--
case3 :: Exp
case3 = tuple3

-- | λxfgh.fx
in13 :: Exp
in13 = abs4 "x" "f" "g" "h" (App (Ref "f") (Ref "x"))

-- | λxfgh.gx
in23 :: Exp
in23 = abs4 "x" "f" "g" "h" (App (Ref "g") (Ref "x"))

-- | λxfgh.hx
in33 :: Exp
in33 = abs4 "x" "f" "g" "h" (App (Ref "h") (Ref "x"))


-- ** Fixpoint combinator

-- | λf. (λx.f(xx)) (λx.f(xx))
fix :: Exp
fix = Abs "f" (App (Abs "x" (App (Ref "f") (App (Ref "x") (Ref "x"))))
                   (Abs "x" (App (Ref "f") (App (Ref "x") (Ref "x")))))

-- | fix (λrn. if (isZero n) one (mult n (r (pred n))))
--
--   >>> num 6 `equiv` eval (App fac three)
--   True
--
--   -- This one is too slow!
--   -- >>> num 24 `equiv` eval (App fac (num 4))
--   -- True
--
fac :: Exp
fac = App fix (abs2 "r" "n"
          (app3 if_
                (App isZero (Ref "n"))  -- condition
                one                     -- base case
                (app2 mult              -- recursive case
                      (Ref "n")
                      (App (Ref "r") (App pred (Ref "n"))))))


-- ** Church-encoded lists

-- | inL (λx.x)
nil :: Exp
nil = App inL (Abs "x" (Ref "x"))

-- | λht. inR (pair h t)
cons :: Exp
cons = abs2 "h" "t" (App inR (app2 pair (Ref "h") (Ref "t")))

-- | fix (λrfbl. either (λx.b) (λp. f (fst p) (r f b (snd p))) l)
fold :: Exp
fold = App fix (abs4 "r" "f" "b" "l"
           (app3 either
                 (Abs "x" (Ref "b"))
                 (Abs "p" (app2 (Ref "f")
                                (App fst (Ref "p"))
                                (app3 (Ref "r") (Ref "f") (Ref "b") (App snd (Ref "p")))))
                 (Ref "l")))

-- | Smart constructor to build a Church-encoded list of natural numbers.
list :: [Int] -> Exp
list = foldr (app2 cons . num) nil

-- | fold (λh. add one) zero
--
--   >>> three `equiv` eval (App length (list [2,3,4]))
--   True
--
length :: Exp
length = app2 fold (Abs "h" (App add one)) zero

-- | fold add zero
--
--   >>> num 9 `equiv` eval (App sum (list [2,3,4]))
--   True
--
sum :: Exp
sum = app2 fold add zero

-- | fold mult one
--
--   >>> num 24 `equiv` eval (App product (list [2,3,4]))
--   True
--
product :: Exp
product = app2 fold mult one

-- | λf. fold (\h. cons (f h)) nil
--
--   >>> eval (list [4,5,6]) `equiv` eval (app2 map' (App add two) (list [2,3,4]))
--   True
--
map' :: Exp
map' = Abs "f" (app2 fold (Abs "h" (App cons (App (Ref "f") (Ref "h")))) nil)
