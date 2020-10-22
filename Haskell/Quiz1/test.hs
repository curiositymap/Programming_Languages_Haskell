data Result a = OK a
              | Error String
show :: Glob -> String
build :: Int -> Glob
const :: a -> b -> a
(.) :: (b -> c) -> (a -> b) -> a -> c

foo :: Glob
OK (show foo) :: Result String

-- data Tree a
--    = Nook a (Tree a)
--    | Node (Tree a) (Tree a)
--    | Leaf
--
-- sumTree :: Tree Int -> Int
-- sumTree Leaf = 0
-- sumTree Nook (Tree a) (Tree a) = h + sumTree t
