{----------------------------------------LEARNING OBJECTIVE
II. Syntax and Naming
1. Given a grammar and program fragment, determine whether the program fragment can be generated from the grammar.
   If so, determine which syntactic category it belongs to.
2. Given a grammar, implement the corresponding abstract syntax as a set of Haskell types and data types.
3. Given an abstract syntax in Haskell, encode a program's AST as a Haskell value.
(4. Determine whether a use of a name is a declaration or reference.)
----------------------------------------------------------}
{--------------------------------------------------EXAMPLE3
Midterm - Part II. Encoding a grammar in Haskell

i ∈ Int   ::=   (any integer)
v ∈ Var   ::=   (any variable name)
e ∈ Exp   ::=   i
           |   v
           |   e + e
           |   e - e
s ∈ Stmt   ::=   v ← e      -- Assign Expr to Var
            |   while e ≤ e -- three arguments for while loop
                do s
            |   begin s*    -- only one argument, s*
                end
----------------------------------------------------------}

data Var
   = VarStr String

data Exp     -- there are four cases in Exp, so PATTERN MATCH
   = Lit Int
   | Get Var
   | Add Exp Exp
   | Sub Exp Exp

data Stmt         -- there are three cases of Stmt
   = Set Var Exp  -- the data type is defined with two arguments: Var & Expr
   | While Exp Exp Stmt
   | BEnd [Stmt]


{--------------------------------------------------EXAMPLE3
Midterm - Part II. Encoding Abstract Syntax Tree in Haskell

Here is an example program written in this language.
The program computes the quotient and remainder of dividing 53 by 8.

begin
    quot ← 0
    rem ← 53
    while 8 ≤ rem do begin
        quot ← quot + 1
        rem ← rem - 8
    end
end
----------------------------------------------------------}

example :: Stmt

example =
  BEnd [
       Set (VarStr "quot") (Lit 0),
       Set (VarStr "rem") (Lit 53),

       While (Lit 8) (Get (VarStr "rem"))
             (BEnd
                 [Set (VarStr "quot") (Add (Get (VarStr "quot")) (Lit 1)),
                  Set (VarStr "rem") (Sub (Get (VarStr "rem")) (Lit 8))
                 ]
             )  -- the third argument of while is Stmt, BEnd [Stmt]
       ]
