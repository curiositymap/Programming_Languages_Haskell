{----------------------------------------LEARNING OBJECTIVE
V. Denotation Semantics and Domain Theory
(1. What elements are in a given semantic domain constructed using lifting, sums, and products?)**
2. What is a good choice of semantic domain for a given language?
3. Implement a simple denotational semantics in Haskell.
----------------------------------------------------------}

{--------------------------------------------------EXAMPLE1
Exercise: Denotational Semantics - DIVISION LANGUAGE

Consider the following language, where
   add represents integer addition, and
   div represents integer division.

Division may fail with an error if the second argument evaluates to 0.

<Syntax>
i  ::=  (any integer)
e  ::=  i
    |   add e e
    |   div e e

1. What is a good choice of semantic domain for expressions in this language?

      Maybe Int


2. Implement the language in Haskell by
    (a) encoding the abstract syntax as a Haskell data type

        data Exp
          = Lit Int
          | Add Exp Exp
          | Div Exp Exp

    (b) implementing a function that maps terms in this language onto values in the semantic domain.

        exp :: Exp -> Maybe Int
        exp (Lit i)   = Just i
        exp (Add l r) = case (exp l, exp r) of
                             (Just i, Just j) -> Just (i+j)
                              _               -> Nothing

        exp (Div l r) = case (exp l, exp r) of
                             (_, Just 0)      -> Nothing
                             (Just i, Just j) -> Just (div i j)   -- div is integer division, from the Prelude
                             _                -> Nothing

----------------------------------------------------------}

{--------------------------------------------------EXAMPLE2
Exercise: Denotational Semantics - COUNTER LANGUAGE

Consider the following language for implementing a simple counter.
Statements either increment (inc) the counter by a given integer, or
they reset the counter to zero.

<Syntax>
i  ::=  (any integer)
s  ::=  inc i
    |   reset
p  ::=  s ; p
    |   ε

A program runs a sequence of statements on an initial counter of 0 and
returns the final value of the counter.

3. What is a good choice of semantic domain for statements in this language?

     Int -> Int.

   What is a good choice of semantic domain for programs?

     Int  (since the spec says the counter starts at 0)

4. Implement the language in Haskell by
    (a) encoding the abstract syntax as a Haskell data type,

        data Stmt
           = Inc Int
           | Reset

        type Prog = [Stmt]

    (b) implementing valuation functions for both statements and programs.

          stmt :: Stmt -> Int -> Int
          stmt (Inc i) c = c+i
          stmt Reset   _ = 0

          -- Helper function for Prog semantics
          stmts :: [Stmt] -> Int -> Int
          stmts []     c = c
          stmts (s:ss) c = stmts ss (stmt s c)
          -- evaluate the segment of it (stmt s c) first, then recursively do the rest stmts ss

          prog :: Prog -> Int
          prog p = stmts p 0

----------------------------------------------------------}
