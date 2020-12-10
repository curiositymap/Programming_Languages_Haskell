{----------------------------------------LEARNING OBJECTIVE
I. Functional Programming
* Determine the type of an expression
  given the types of any relevant functions and values.
* Construct an expression of the given type from the provided values.
----------------------------------------------------------}

{--------------------------------------------------EXAMPLE1
Given the type definitions,
Pt. 1 - Infer the type of sub-expressions &
Pt. 2 - Create expressions of the indicated types
----------------------------------------------------------}


import Prelude hiding (show, const, (.))

{- Type Definitions -}

data Result a
  = OK a
  | Error String

data Glob = Glob

show :: Glob -> String
show = undefined

build :: Int -> Glob
build = undefined

const :: a -> b -> a
const = undefined

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = undefined


{- Part 1. Infer the type of sub-expressions

Q1.

OK (show foo) :: Result String
OK a          :: Result a

Therefore,
(show foo) :: String
 show      :: Glob -> String

Therefore,
foo :: Glob


Q2.

const bar (build 3) :: Int
const :: a -> b -> a

build :: Int -> Glob
build 3 :: Glob

Therefore, a = Int, b = Glob
We can infer that
bar :: Int


Q3.

show (.) baz :: String -> String

Using the pipeline approach,
baz :: String -> Glob
show :: Glob -> String

-}

{- Part 2. Create an expression of the indicated type

Q4.

??? :: Result (Int -> Glob)

OK :: a -> Result a
Therefore, a = Int -> Glob

build :: Int -> Glob
OK (build)

Check the answer by typing
:t OK (build)


Q5.

??? :: Int -> String

We can infer it's a function composition case.
Use the pipeline approach. (e.g., f . g)

g :: Int -> ???
f :: ??? -> String

build :: Int -> Glob
show  :: Glob -> String

Therefore, the answer is

show . build

Check the answer by typing
:t show . build


Q6.

??? :: Glob -> Result a

We can infer it's a function composiotion case.
Use the pipeline approach. (e.g., f . g)

g :: Glob -> ???
f :: ???  -> Result a

show  :: Glob   -> String
Error :: String -> Result a

Therefore, the answer is

Error . show

Check the answer by typing
:t Error . show

-}
