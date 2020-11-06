
{- Part I. Implementing Recursive Function -}

data Tree a
   = Nook a (Tree a)
   | Node (Tree a) (Tree a)
   | Leaf

sumTree :: Tree Int -> Int
sumTree Leaf = 0
sumTree (Nook i t) = i + sumTree t
sumTree (Node l r) = sumTree l + sumTree r


incTree :: Int -> Tree Int -> Tree Int
incTree i Leaf = i
incTree i (Nook a b) = a + (i + incTree b)
incTree i (Node l r) = (i + incTree l) + (i + incTree r)

{- Part II. Type Inference -}

data Result a
   = OK a
   | Error String

data Glob
   = Lit Int

show :: Glob -> String
show = undefined

build :: Int -> Glob
build = undefined

const :: a -> b -> a
const = undefined

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = undefined

-- Question 1. Type of `foo`
OK (show foo) :: Result String
foo :: Glob

-- Queston 2. Type of `bar`
-- Be careful!
const bar (build 3) :: Int
bar :: Int

-- Question 3. Type of `baz`
show. baz :: String -> String
baz :: String -> Glob

-- Question 4.
OK build :: Result (Int -> Glob)

-- Question 5.
show . build :: Int -> String

-- Question 6.
-- Be careful!
Error . show :: Glob -> Result a
