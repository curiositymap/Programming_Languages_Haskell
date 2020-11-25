-- | An illustration of encoding a small Haskell program in lambda calculus.
{- Church Encoding Rules
1. At least, we need FUNCTIONS that encode
1-1. Construct new values of the data type
1-2. Destruct & user values of the data type in a general way

2. Many data types can be encoded into sums (Either) or products (tuples)
2-1. sums (Either)
     data Val = A Nat | B Bool | C Nat Bool
2-2. products (tuples)
     type Val' = Either Nat (Either Bool (Nat, Bool))


-}


module HaskellToChurch where

import Prelude hiding (print,and,or,not,pred,succ,fst,snd,either,length,sum,product)
import qualified Prelude as Prelude

import LambdaCalculus


--
-- * Original Haskell program
--

-- | Pretend Haskell's Int is restricted to Nats.
type Nat = Int

-- | A simple data type with three cases.
data Val = A Nat | B Bool | C Nat Bool
  deriving (Eq,Show)

-- | An arbitrary function that pattern matches on Val.
foo :: Val -> Nat
foo (A n)   = n
foo (B b)   = if b then 0 else 1
foo (C n b) = if b then 0 else n

-- | Some example values.
val1, val2, val3 :: Val
val1 = A 2
val2 = B True
val3 = C 3 False


--
-- * Church encoding in lambda calculus
--

-- | Example values encoded in lambda calculus.
exp1, exp2, exp3 :: Exp
exp1 = App in13 two
exp2 = App in23 true
exp3 = App in33 (app2 pair three false)

-- | Haskell function that converts a Val into an equivalent value
--   encoded in lambda calculus.
encodeVal :: Val -> Exp
encodeVal (A n)   = App in13 (num n)
encodeVal (B b)   = App in23 (if b then true else false)
encodeVal (C n b) = App in33 (app2 pair (num n) (if b then true else false))

-- | The foo function encoded in lambda calculus.
fooExp :: Exp
fooExp = Abs "v" (app4 case3
           (Abs "n" (Ref "n"))
           (Abs "b" (app3 if_ (Ref "b") zero one))
           (Abs "p" (app3 if_ (App snd (Ref "p")) zero (App fst (Ref "p"))))
           (Ref "v"))
-- fooExp = app3 case3
--            (Abs "n" (Ref "n"))
--            (Abs "b" (app3 if_ (Ref "b") zero one))
--            (Abs "p" (app3 if_ (App snd (Ref "p")) zero (App fst (Ref "p"))))
