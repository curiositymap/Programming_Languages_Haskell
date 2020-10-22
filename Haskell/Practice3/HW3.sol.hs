module HW3 where

import Data.List (group)
import Prelude hiding (Enum(..), sum)


--
-- * Part 1: Run-length lists
--

-- | Convert a regular list into a run-length list.
--
--   >>> compress [1,1,1,2,3,3,3,1,2,2,2,2]
--   [(3,1),(1,2),(3,3),(1,1),(4,2)]
-- 
--   >>> compress "Mississippi"
--   [(1,'M'),(1,'i'),(2,'s'),(1,'i'),(2,'s'),(1,'i'),(2,'p'),(1,'i')]
--
compress :: Eq a => [a] -> [(Int,a)]
compress = map entry . group 
  where entry xs = (length xs, head xs)

compress' :: Eq a => [a] -> [(Int,a)]
compress' []     = []
compress' (x:xs) = let (c,r) = count x xs
                   in (c,x) : compress' r
  where
    count :: Eq a => a -> [a] -> (Int,[a])
    count y []     = (1,[])
    count y (z:zs) =
      if y == z
      then let (c,r) = count y zs
           in (c+1,r)
      else (1,z:zs)

-- | Convert a run-length list back into a regular list.
--
--   >>> decompress [(5,'a'),(3,'b'),(4,'c'),(1,'a'),(2,'b')]
--   "aaaaabbbccccabb"
--  
decompress :: [(Int,a)] -> [a]
decompress = concatMap (uncurry replicate)

-- decompress = concatMap (\(n,a) -> replicate n a)

-- decompress []        = []
-- decompress ((n,a):t) = replicate n a ++ decompress t


--
-- * Part 2: Natural numbers
--

-- | The natural numbers.
data Nat
   = Zero
   | Succ Nat
  deriving (Eq,Show)

-- | The number 1.
one :: Nat
one = Succ Zero

-- | The number 2.
two :: Nat
two = Succ one

-- | The number 3.
three :: Nat
three = Succ two

-- | The number 4.
four :: Nat
four = Succ three


-- | The predecessor of a natural number.
--   
--   >>> pred Zero
--   Zero
--   
--   >>> pred three
--   Succ (Succ Zero)
--   
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ n) = n


-- | True if the given value is zero.
--
--   >>> isZero Zero
--   True
--
--   >>> isZero two
--   False
--
isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False


-- | Convert a natural number to an integer. NOTE: We use this function in
--   tests, but you should not use it in your other definitions!
--
--   >>> toInt Zero
--   0
--
--   >>> toInt three
--   3
--
toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n


-- | Add two natural numbers.
--
--   >>> add one two
--   Succ (Succ (Succ Zero))
--
--   >>> add Zero one == one
--   True
--
--   >>> add two two == four
--   True
--
--   >>> add two three == add three two
--   True
--   
add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = add n (Succ m)


-- | Subtract the second natural number from the first. Return zero
--   if the second number is bigger.
--
--   >>> sub two one
--   Succ Zero
--   
--   >>> sub three one
--   Succ (Succ Zero)
--
--   >>> sub one one
--   Zero
--
--   >>> sub one three
--   Zero
--
sub :: Nat -> Nat -> Nat
sub Zero     _        = Zero
sub n        Zero     = n
sub (Succ n) (Succ m) = sub n m


-- | Is the left value greater than the right?
--
--   >>> gt one two
--   False
--
--   >>> gt two one
--   True
--
--   >>> gt two two
--   False
--
gt :: Nat -> Nat -> Bool
gt Zero     _        = False
gt _        Zero     = True
gt (Succ n) (Succ m) = gt n m


-- | Multiply two natural numbers.
--
--   >>> mult two Zero
--   Zero
--
--   >>> mult Zero three
--   Zero
--
--   >>> toInt (mult two three)
--   6
--
--   >>> toInt (mult three three)
--   9
--
mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ n) m = add m (mult n m)

-- mult (Succ (Succ Zero)) four
-- => add four (mult (Succ Zero) four)
-- => add four (add four (mult Zero four))
-- => add four (add four Zero)


-- | Compute the sum of a list of natural numbers.
--
--   >>> sum []
--   Zero
--   
--   >>> sum [one,Zero,two]
--   Succ (Succ (Succ Zero))
--
--   >>> toInt (sum [one,two,three])
--   6
--
sum :: [Nat] -> Nat
sum = foldr add Zero


-- | An infinite list of all of the *odd* natural numbers, in order.
--
--   >>> map toInt (take 5 odds)
--   [1,3,5,7,9]
--
--   >>> toInt (sum (take 100 odds))
--   10000
--
odds :: [Nat]
odds = one : map (add two) odds

-- one : map (add two) odds
--                     ----
-- => one : map (add two) (one : map (add two) odds)
--          ----------------------------------------                     
-- => one : add two one : map (add two) (map (add two) odds)
--                                                     ----
-- => one : add two one : map (add two) (map (add two) (one : map (add two) odds))
--                                      ------------------------------------------
-- => one : add two one : map (add two) (add two one : map (add two) (map (add two) odds))
--                        ----------------------------------------------------------------
-- => one : add two one : add two (add two one) : map (add two) (map (add two) (map (add two) odds))

-- map _ []     = []
-- map f (x:xs) = f x : map f xs
