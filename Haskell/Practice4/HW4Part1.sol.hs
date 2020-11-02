module HW4Part1 where


-- 1. Abstract syntax

data Reg
  = A
  | B -- A, B, C are all data constructs with zero argument
  | R
  deriving (Eq,Show)

-- You MUST have a data constructor for any argument / each case
-- Each data constructor can take zero or more arguments

data Expr    -- Expr is a type constructor
   = Lit Int -- Lit is a data constructor, Int is argument
   | Get Reg
   | Add Expr Expr
   | LTE Expr Expr
   | Not Expr
  deriving (Eq,Show)

data Stmt
   = Set Reg Expr
   | If Expr Prog Prog
   | Do Prog
   | Break
  deriving (Eq,Show)

type Prog = [Stmt]


-- 2. Example program
example :: Prog
example =
  [ Set A (Lit 7)
  , Set B (Lit 9)
  , Set R (Lit 0)
  , Do
    [ If (LTE (Get A) (Lit 0))
      [ Break ]
      [ Set R (Add (Get R) (Get B))
      , Set A (Add (Get A) (Lit (-1)))
      ]
    ]
  ]


-- 3. While loop syntactic sugar.
while :: Expr -> Prog -> Stmt
while c b = Do [If c b [Break]]


-- 4. Program that sums numbers from x to y (loop in the object language).
sumFromTo :: Int -> Int -> Prog
sumFromTo x y =
  [ Set R (Lit 0)
  , Set A (Lit x)
  , while (LTE (Get A) (Lit y))
    [ Set R (Add (Get R) (Get A))
    , Set A (Add (Get A) (Lit 1))
    ]
  ]

-- 4. Program that sums numbers from x to y (loop in metalanguage).
sumFromTo' :: Int -> Int -> Prog
sumFromTo' x y = Set R (Lit 0) : map add [x..y]
  where add i = Set R (Add (Get R) (Lit i))
