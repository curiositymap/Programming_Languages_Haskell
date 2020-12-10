import Prelude hiding (show, const, (.), flip)

{- Type Definitions -}

data Cmd
   = Push Int
   | Add
   | Check Int Cmd

data Stack = Stack

empty :: Stack
empty  = undefined

arity   :: Cmd -> Int
arity   = undefined

execute :: Cmd -> Stack -> Stack
execute = undefined

flip    :: (a -> b -> c) -> b -> a -> c
flip    = undefined

(.)     :: (b -> c) -> (a -> b) -> a -> c
(.)     = undefined
