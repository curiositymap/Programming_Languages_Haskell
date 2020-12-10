{----------------------------------------LEARNING OBJECTIVE
II. Syntax and Naming
1. Given a grammar and program fragment, determine whether the program fragment can be generated from the grammar.
   If so, determine which syntactic category it belongs to.
2. Given a grammar, implement the corresponding abstract syntax as a set of Haskell types and data types.
3. Given an abstract syntax in Haskell, encode a program's AST as a Haskell value.
(4. Determine whether a use of a name is a declaration or reference.)
----------------------------------------------------------}

{--------------------------------------------------EXAMPLE1
Quiz2 - Part I. Encoding a grammar in Haskell

Given a grammar describing the concrete syntax of a small PL,
define the abstract syntax as a set of Haskell 'data' and 'type' definitions.
(You can use Haskell's Int type directly.)

<Grammar>
i ∈ Int   ::=   (any integer)

r ∈ Reg   ::=   zero
           |   reg i
e ∈ Exp   ::=   i
           |   r
           |   e + e
c ∈ Cmd   ::=   print e
           |   set r to e      -- two arguments, r and e
           |   loop e do p end -- two arguments, e and p
p ∈ Prog   ::=   ε
           |   c ; p
----------------------------------------------------------}

-- Each line defined in the grammar requires a data constructor.
-- Pattern match is needed in this case.

data Reg
   = Zero       -- no argument, just data constructor
   | RegInt Int -- designate register name

data Exp
   = LitInt Int
   | Load Reg
   | Add Exp Exp

data Cmd
   = Print Exp
   | Set Reg Exp
   | Loop Exp Prog

type Prog = [Cmd]

{--------------------------------------------------EXAMPLE1
Quiz2 - Part II. Encoding Abstract Syntax Tree in Haskell

Determine whether each string below is a program that can be genearted by the grammar.
For each problem below, define a Haskell value of the indicated name.
----------------------------------------------------------}

-- ex1: set reg 2 to zero --> command
ex1 :: Cmd
ex1 = Set (RegInt 2) (Load Zero)

-- ex2: loop 3 do print 4; print 5; end --> Loop (3) [Print 4, Print 5]
ex2 :: Cmd
ex2 = Loop (LitInt 3) [Print (LitInt 4), Print (LitInt 5)]

-- ex3: set reg 6 to print 7 --> Set Reg Exp --> Set (RegInt 6) (Exp)
ex3 :: Cmd
ex3 = undefined

-- ex4: print 8 + reg 9;
-- Command
-- Print Exp
-- Print (Add (LitInt 8) + Load (RegInt 9)))
ex4 :: Cmd
ex4 = Print (Add (LitInt 8) (Load (RegInt 9)))
