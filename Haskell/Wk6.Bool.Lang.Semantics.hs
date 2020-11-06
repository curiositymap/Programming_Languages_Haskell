-- Structural Operational Semantics Implementations Using Haskell Function

module Bool where


-- | Syntax of the simple Boolean expression language.
data Exp
   = Tru
   | Fls
   | Not Exp
   | If Exp Exp Exp
  deriving (Eq,Show)

-- | Big-step semantics.
eval :: Exp -> Exp
eval Tru        = Tru
eval Fls        = Fls
eval (Not e)    = case eval e of
                    Tru -> Fls
                    Fls -> Tru
                    _   -> error "eval: whoops"
                    -- The third line is optional:
                    -- Since Exp has multiple cases beyond just Tru and Fls,
                    -- It is recommended to "Pattern Match" other cases such as
                    -- Not Exp and If Exp Exp Exp by using _

eval (If c t e) = case eval c of
                    Tru -> eval t
                    Fls -> eval e
                    _   -> error "eval: whoops"
                    -- The third line is optional:
                    -- Since Exp has multiple cases beyond just Tru and Fls,
                    -- It is recommended to "Pattern Match" other cases such as
                    -- Not Exp and If Exp Exp Exp by using _

-- | One-step reduction relation for the small-step semantics. (Full semantics)
step :: Exp -> Exp
-- reflexive rules (not actually part of the relation)
step Tru          = Tru
step Fls          = Fls
-- These two lines above are optional
-- Just for "Pattern Match"


-- reduction rules
step (Not Tru)    = Fls
step (Not Fls)    = Tru
step (If Tru t e) = t
step (If Fls t e) = e
-- congruence rules
step (Not e)      = Not (step e)
step (If c t e)   = If (step c) t e

-- | Reflexive, transitive closure of the one-step reduction.
steps :: Exp -> Exp
-- reflexive rules for values -- for the final cases
steps Tru = Tru
steps Fls = Fls
-- transitivity rule for non-values
steps e   = steps (step e)

-- Since it is nice to see the steps required to reduce an expression -- we created a helper function
-- | Produce the reduction sequence for an expression.
-- Collect all the intermediatery steps
-- "reduce" function works the same way as steps
reduce :: Exp -> [Exp]
reduce Tru = [Tru]
reduce Fls = [Fls]
reduce e   = e : reduce (step e)

-- test this command:
-- putStrLn $ unlines $ map show $ reduce (Not (Not (If (Not Tru) (Not Fls) (Not Tru))))
-- putStrLn $ unlines $ map show $ reduce (Not (Not (If (Not Tru) (Not Fls) (Not Tru))))
