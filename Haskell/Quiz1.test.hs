-- data Result a = OK a
--               | Error String
-- show :: Glob -> String
-- build :: Int -> Glob
-- const :: a -> b -> a
-- (.) :: (b -> c) -> (a -> b) -> a -> c
--
-- foo :: Glob
-- OK (show foo) :: Result String

-- data Tree a
--    = Nook a (Tree a)
--    | Node (Tree a) (Tree a)
--    | Leaf
--
-- sumTree :: Tree Int -> Int
-- sumTree Leaf = 0
-- sumTree (Nook i t) = i + sumTree t
-- sumTree (Node l r) = sumTree l + sumTree r

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

con
