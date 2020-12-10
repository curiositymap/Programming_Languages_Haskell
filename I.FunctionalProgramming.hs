{----------------------------------------LEARNING OBJECTIVE
I. Functional Programming
* Define a Haskell value or function from a specification
----------------------------------------------------------}


{--------------------------------------------------EXAMPLE1
Quiz 1 - Part I. Implement a recursive function

Consider the following definition of a binary tree data type
that stores elements only in "nooks".

Define a function 'sumTree' that sums all of the elements
in a tree of type 'Tree Int'.
----------------------------------------------------------}

data Tree a
   = Nook a (Tree a)        -- one-sided branch
   | Node (Tree a) (Tree a) -- two-sided  branches
   | Leaf                   -- nothing (terminal)
   deriving (Eq, Show)


sumTree :: Tree Int -> Int
sumTree (Nook h t) = h + sumTree t
sumTree (Node l r) = sumTree l + sumTree r
sumTree Leaf       = 0

-- User PATTERN MATCHING for each case of data type Tree


{--------------------------------------------------EXAMPLE2
Midterm - Part I. Implement a recursive function

Define a function 'incTree' that takes an integer i and a tree t &
return a new tree that is identical to t except that
every integer it contains has been incremented by i.
----------------------------------------------------------}

incTree :: Int -> Tree Int -> Tree Int
incTree i (Nook h t) = Nook (h+i)         (incTree i t)
incTree i (Node l r) = Node (incTree i l) (incTree i r)
incTree i Leaf       = Leaf

-- User PATTERN MATCHING for each case of data type Tree


ex1, ex2 :: Tree Int
ex1 = Node (Nook 3 Leaf) (Nook 5 Leaf)
ex2 = Nook 4 (Node (Nook 7 Leaf) (Node (Nook 6 Leaf) Leaf))

{- Test
> incTree 10 ex1
Node (Nook 13 Leaf) (Nook 15 Leaf)

> incTree 100 ex1
Node (Nook 103 Leaf) (Nook 105 Leaf)

> incTree 10 ex2
Nook 14 (Node (Nook 17 Leaf) (Node (Nook 106 Leaf) Leaf))

-}
