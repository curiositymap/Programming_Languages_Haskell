data Tree a
   = Nook a (Tree a)
   | Node (Tree a) (Tree a)
   | Leaf
   deriving (Eq,Show)

-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree _ Leaf = Leaf
-- -- mapTree f (Nook i t) = Nook i (mapTree f t)
-- mapTree f (Node l r) = Node (mapTree f l) (mapTree f r)
--
--
-- treeIncr :: Int -> Tree Int -> Tree Int
-- treeIncr a = treeMap (+a)

ex1, ex2 :: Tree Int
ex1 = Node (Nook 3 Leaf) (Nook 5 Leaf)
ex2 = Nook 4 (Node (Nook 7 Leaf) (Node (Nook 6 Leaf) Leaf))

incTree :: Int -> Tree Int -> Tree Int
incTree i Leaf = Leaf
incTree i (Nook a b) = Nook (a + i) (incTree i b)
incTree i (Node l r) = Node (incTree i l) (incTree i r)



-- sumTree :: Tree Int -> Int
-- sumTree Leaf = 0
-- sumTree (Nook i t) = i + sumTree t
-- sumTree (Node l r) = sumTree l + sumTree r


-- instance Functor Tree where
--     fmap = mapTree
--
-- data Tree a
--    = Empty
--    | Node a (Tree a) (Tree a)
--
-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree _ Empty = Empty
-- mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)
--
-- instance Functor Tree where
--     fmap = mapTree
--
--
--
--
-- data Tree a
--    = Nook a (Tree a)
--    | Node (Tree a) (Tree a)
--    | Leaf
--
--
--
--
-- data Tree' a
--    = Node' a (Tree a) (Tree a)
--    | Leaf'
--
-- ex1, ex2 :: Tree Int
-- ex1 = Node (Nook 3 Leaf) (Nook 5 Leaf)
-- ex2 = Nook 4 (Node (Nook 7 Leaf) (Node (Nook 6 Leaf) Leaf))
--
-- -- sumTree :: Tree Int -> Int
-- -- sumTree Leaf = 0
-- -- sumTree (Nook i t) = i + sumTree t
-- -- sumTree (Node l r) = sumTree l + sumTree r
--
--
-- treeFold :: (a -> b -> b) -> b -> Tree a -> b
-- treeFold _ base Leaf         = base
-- treeFold f base (Nook i t) = f i (treeFold f base t)
-- treeFold f base (Node l r) = f (treeFold f base l) (treeFold f base r)
--
-- treeMap :: (a -> b) -> Tree a -> Tree b
-- treeMap f = treeFold (Node . f) Leaf
--
-- treeIncr :: Int -> Tree Int -> Tree Int
-- treeIncr a = treeMap (+a)
-- --
-- --
-- -- treemap f tree = go tree
-- --   where go (Leaf a)      = Leaf (f a)
-- --         go (Node xl xr)  = Node (go xl) (go xr)
-- --
-- --
-- -- incTree :: Int -> Tree Int -> Tree Int
-- -- incTree i Leaf = i
-- -- incTree i (Nook a b) = a + (i + incTree b)
-- -- incTree i (Node l r) = (i + incTree l) + (i + incTree r)
