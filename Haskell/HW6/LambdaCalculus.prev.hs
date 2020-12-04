{-
import Data.Set (Set)
import qualified Data.Set as Set

These two lines above should be manually typed & loaded at the beginning of running GHCI
for some reason (maybe due to the ghci compatibility issues).

-}

printReduce step = mapM_ print . steps safeSub step

-- | Print a normal-order reduction sequence for an expression.
printReduceN :: Exp -> IO ()
printReduceN = printReduce stepN

-- | Print an applicative-order reduction sequence for an expression.
printReduceA :: Exp -> IO ()
printReduceA = printReduce stepA


-- ** Examples and tests

-- | Variable capture examples from slide 17.
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

-- | λfx.x
zero :: Exp
zero = abs2 "f" "x" (Ref "x")

-- | λfx.fx
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

-- | λnfx.f(nfx)
succ :: Exp
succ = abs3 "n" "f" "x" (App (Ref "f") (app2 (Ref "n") (Ref "f") (Ref "x")))

-- | λnmfx.nf(mfx)
add :: Exp
add = abs4 "n" "m" "f" "x" (app2 (Ref "n") (Ref "f") (app2 (Ref "m") (Ref "f") (Ref "x")))

-- | λnmf.n(mf)
mult :: Exp
mult = abs3 "n" "m" "f" (App (Ref "n") (App (Ref "m") (Ref "f")))

-- | λn. n (λx.false) true
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
