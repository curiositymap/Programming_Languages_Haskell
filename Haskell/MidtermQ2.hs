{- Grammar --> syntax -}

{-
Given the grammar below, define a syntax.

i ∈ Int   ::=   (any integer)
v ∈ Var   ::=   (any variable name)
e ∈ Exp   ::=   i   |   v   |   e + e   |   e - e
s ∈ Stmt   ::=   v ← e   |   while e ≤ e do s   |   begin s* end

Given a programming below, create an AST using Haskell Type and Data Types.

begin
    quot ← 0
    rem ← 53
    while 8 ≤ rem do begin
        quot ← quot + 1
        rem ← rem - 8
    end
end

 
-}

-- Caveats : Var can be any "STRING"
-- When you're defining the AST of the given program,
-- Make sure to use "STRING" to denote STRING!!!

data Var
   = VarLit String

data Exp
   = Lit Int
   | Get Var
   | Add Exp Exp
   | Sub Exp Exp

data Stmt
   = Set Var Exp
   | While Exp Exp Stmt
   | BEnd [Stmt]

example :: Stmt

example =
  BEnd
  [ Set (VarLit "quot") (Lit 0)
  , Set (VarLit "rem") (Lit 53)
  , While
        (Lit 8) (Get (VarLit "rem"))
        BEnd
        [
          Set (VarLit "quot") (Add (Get (VarLit "quot")) (Lit 1))
        , Set (VarLit "rem") (Sub (Get (VarLit "rem")) (Lit 8))
        ]
       ]
