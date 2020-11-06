module StackLang where

import Prelude hiding (Num)

--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
--
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num           push a number on the stack
--         |  bool          push a boolean on the stack
--         |  `+`           add the top two numbers on the stack
--         |  `*`           multiply the top two numbers on the stack
--         |  `<=`          is the top number LEQ the second number on the stack
--         |  `if`   prog   if the value on the top is true, then run
--            `else` prog   the first program, else run the second
--            `end`


-- 1. Encode the above grammar as a set of Haskell data types

type Num = Int

type Prog = [Cmd]

data Cmd
   = PushN Num
   | PushB Bool
   | Add
   | Mul
   | LEq
   | IfElse Prog Prog
  deriving (Eq,Show)


-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 + 5 <=
--
ex1 :: Prog
ex1 = [PushN 3, PushN 4, Add, PushN 5, LEq]


-- 3. Write a StackLang program that:
--     * checks whether 3 is less than or equal to 4
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax
--    as a Haskell value.
--
--    4 3 <= if 5 6 + else false end
--
ex2 :: Prog
ex2 = [PushN 4, PushN 3, LEq,
       IfElse [PushN 5, PushN 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack.
addXY :: Num -> Num -> Prog
addXY x y = [PushN x, PushN y, Add, Add]
-- addXY x y = [PushN x, Add, PushN y, Add]


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that computes the sum of all the
--    integers.
sumNs :: [Int] -> Prog
sumNs []     = [PushN 0]
sumNs (i:is) = sumNs is ++ [PushN i, Add]
-- sumNs (i:is) = (PushN i : sumNs is) ++ [Add]


--
-- * Operational Semantics of StackLang (now!)

-- 6. Identify/define the machine state for a StackLang program.

-- To identify a machine state, we need more than just expression
-- Our machine state needs to know the "Stack"
-- Therefore, let's define a LIST of "Int & Bool"
-- Something like type Stack = [Int & Bool]

-- To that end, Haskell has Either data type in Prelude that looks like this:
---- data Either a b
----   = Left a -- constructor a
----   | Right b -- constructor b

{- Example: A stack containing both Int and Bool
[True, 3, 4] -- a list composed of Int & Bool --> Type error
-- To fix this problem, use Either and specify the data type
[Left True, Right 3, Right 4]
-}

type Stack = [Either Bool Int]

-- In addition to the "Stack" we need a program as a pair of Program & Stack
type State = (Prog, Stack)
-- State is a tuple of Prog and Stack





-- 7. Define a one-step reduction relation for a StackLang program,
--    and implement it as a function.

step :: State -> Maybe State
-- Maybe State --> so that we can represent failure cases
-- "Pattern match" on all of the cases

-- <REDUCTION RULES>

step (PushN n : p, s) = Just (p, Right n : s)
-- Since we used Maybe, return type should match as Just

step (PushB b : p, s) = Just (p, Left b : s)
-- Same as the above cas PushN (Push Int to Stack case),
-- but use Left constructor since it's boolean

step (Add : p, Right n : Right m : s) = Just (p, Right (n + m) : s)
-- Specify the type Right since the input elements should be Int
-- Add the new element (n+m) using Cons (:) to the Stack at the beginning

step (Mul : p, Right n : Right m : s) = Just (p, Right (n * m) : s)
-- Same as the above case Add
-- Add the new element (n*m) using Cons (:) to the stack

step (LEq : p, Right n : Right m : s) = Just (p, Left (n <= m) : s)
-- Same as the above case input elements are Int
-- but use Left constructor since the added element to the Stack is Bool

{- IfElse Conditional -}
-- Divide the cases of elements at the top of the stack (Bool)
-- When Left True is on top of the stack, take the first program (t)
-- When Left False is on top of the stack, take the second program (e)

step (IfElse t e : p, Left True  : s) = Just (t ++ p, s)
-- Why ++ ? `++` operator is the list concatenation operator
-- that takes TWO lists and combine them into a SINGLE list.
-- t and p are both programs (list of commands)
-- this is going to replace IfElse branch with t
-- "++" operator is the list concatenation operator
-- that takes TWO lists and combine them into a SINGLE list.

step (IfElse t e : p, Left False : s) = Just (e ++ p, s)
-- Divide the cases of elements at the top of the stack (Bool)
-- When Left True is on top of the stack, take the first program (t)
-- When Left False is on top of the stack, take the second program (e)

-- no congruence rules!
-- All of the other cases
-- error cases
step _ = Nothing

{- We only have reduction rules, no congruence rules.
No need to look into the stack for congruence rules. -}


{- Once you compile the script/load StackLang.hs,
let's type the commands below:

**** Make sure to use the data constructor "Just"  ****
**** to pattern-match step :: State -> Maybe State ****

> Just u = step ([PushN3, PushN 4, Add], [Right 5])
> u
> ([PushN 4,Add],[Right 3,Right 5])

> Just u' = step u
> u'
> ([Add],[Right 4,Right 3,Right 5])

> Just u'' = step u'
> u''
> ([], [Right 7, Right 5])

cf. When we enter the command below:
> step([Add], [Right 4])
> Nothing
-- This is due to the fact that Add program requires two arguments in the stack

-}

-- 8. Implement the reflexive, transitive closure of the one-step
--    reduction to evaluate a StackLang program.

steps :: State -> Maybe State

-- reflexive case
steps ([],s) = Just ([],s)
-- When our program is emtpy, we'll return the stack unchanged.


-- transitive case
{-
steps u = steps (step u)

-- Type error indicates that the function expects 'State'
-- but the input is Maybe State, so we need to pattern match.
-}

steps u      = case step u of
                 Just u' -> steps u'
                 Nothing -> Nothing

                -- We need to pattern match by specifying the constructors
                -- Such as Just or Nothing


{-
Enter the commands below:

> steps ([PushN 3, PushN 5, Add], [])
> Just ([], [Right 8])

-- Applied the step operation three steps
-- Once a program is done, it needs to be empty.

-}
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

-- Helper function that runs a program on an emtpy stack
-- and returns the final stack until the program is empty.
runOpSem :: Prog -> Maybe Stack
runOpSem p = case steps (p,[]) of
               Just ([],s) -> Just s
               _ -> Nothing


{- Let's test by running the commands below:
> runOpSem ex2
> Just [Right 11]

> sumNs [1..10] -- sum the integers from 1 to 10
> [PushN 0,PushN 10,Add,PushN 9,Add,PushN 8,Add,PushN 7,Add,PushN 6,Add,PushN 5,Add,PushN 4,Add,PushN 3,Add,PushN 2,Add,PushN 1,Add]

> runOpSem $ sumNs [1..100]
> Just [Right 5050]
}

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
