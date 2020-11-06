

data Var
   = VarLit Char

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
  BEnd [ Set (VarLit quot) (Lit 0)
       , Set (VarLit rem) (Lit 53)
       , While
        (Lit 8) (Get rem)
        BEnd [
          Set (VarLit quot) (Add (Get quot) (Lit 1))
        , Set (VarLit rem) (Sub (Get rem) (Lit 1))
        ]
       ]
