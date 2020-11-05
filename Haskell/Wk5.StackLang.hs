module StackLang where


import Prelude hiding (Num)


{- stack language is awesome! application includes Postscript, printers, and calculators.
The main benefit of stack language is the ease of parsing.
You don't have to parse anything. Therefore, this language is suitable for applications with limited memory. -}

--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
--
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num                         push a number on the stack
--         |  bool                        push a boolean on the stack
--         |  `add`                       add the top two numbers on the stack
--         |  `mul`                       multiply the top two numbers on the stack
--         |  `eq`                        check whether the top two elements are equal
--         |  `if` prog `else` prog `end` if the value on the top is true
--                                        then run the first program, else run the second


-- 1. Encode the above grammar as a set of Haskell data types

type Num = Int

type Prog = [Cmd]

data Cmd
   = PushN Num -- we need a data constructor for Num and Bool (non-terminal)
   | PushB Bool -- Num and Bool are already defined in Haskell, so we don't have to redefine them.
   | Add
   | Mul
   | LEq
   | IfElse Prog Prog
   deriving (Eq, Show)

-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 eq
--
-- | Think about the metalanguage and Haskell language
ex1 :: Prog
ex1 = [PushN 3, PushN 4, Add, PushN 5, LEq]


-- 3. Write a StackLang program that:
--     * checks whether 3 and 4 are equal
---------- Push 4, 3, <= (Due to the nature of push stack programming, compute inversely)
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
----------- 4 3 <= if 5 6 + else false end (Concrete Syntax)
--    First write it in concrete syntax (Grammar), then in abstract syntax (Haskell)
--    as a Haskell value.
--
--
--
ex2 :: Prog
ex2 = [PushN 4, PushN 3, LEq, IfElse [PushN 5, PushN 6, Add] [PushB False]]
-- This is a tree structure with a abstract syntax list


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack.
addXY :: Num -> Num -> Prog
addXY x y = [PushN x, PushN y, Add, Add]
-- alternative solution: addXY x y = [PushN x, Add, PushN y, Add]

-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that computes the sum of all the integers.
sumNs :: [Int] -> Prog
sumNs []     = [PushN 0]
sumNs (i:is) = sumNs is ++ [PushN i, Add] -- this will fluctuate between push and add by only using a couple of stack
-- sumNs (i:is) = (PushN i : sumNs is) ++ [Add] -- this will grow the stack and reduce the stack


--
-- * Operational Semantics of StackLang (later)
--

-- 6. Identify/define the machine state for a StackLang program.


-- 7. Define a one-step reduction relation for a StackLang program,
--    and implement it as a function.
step = undefined


-- 8. Implement the reflexive, transitive closure of the one-step
--    reduction to evaluate a StackLang program.
steps = undefined


-- | Run a program on an initially empty stack.
--
--   >>> runOpSem ex2
--   Just [Right False]
--
--   >>> runOpSem (sumNs [1..10])
--   Just [Left 55]
--
--   >>> runOpSem [PushN 3, Add, PushN 4]
--   Nothing
--
runOpSem = undefined



--
-- * Denotational Semantics of StackLang (even later)
--

-- 9. Identify/define a semantics domain for Cmd and for Prog.


-- 10. Define the semantics of a StackLang command (ignore If at first).
cmd = undefined

-- 11. Define the semantics of a StackLang program.
prog = undefined


-- | Run a program on an initially empty stack.
--
--   >>> runDenSem ex2
--   Just [Right False]
--
--   >>> runDenSem (sumNs [1..10])
--   Just [Left 55]
--
--   >>> runDenSem [PushN 3, Add, PushN 4]
--   Nothing
--
runDenSem = undefined
