{----------------------------------------LEARNING OBJECTIVE
III. Inference Rules and Operational Semantics
(1. Given a judgement defined using inference rules and a claim, construct a direct proof (i.e. proof tree) of the claim.)**
(2. Given a natural semantics of a language, write a proof tree showing the evaluation of a term.)**
3. Given a structural operational semantics for a language defined using inference rules,
   write the reduction sequence of a term.
----------------------------------------------------------}

{--------------------------------------------------EXAMPLE1
Quiz 3 - Lambda Calculus
Operational Semantics: How to evaluate a program?

<Reduction Strategies>
STEPS:
1. Identify the redexes
2. Figure out the leftmost redex (Normal-order)
2. Figure out the leftmost of the innermost redex(Applicative-order)

Normal-order vs. Applicative-order reduction
-- NO (Lazy evaluation): it will do exactly what it's needed to do -- time performance, difficult to predict on space usage
-- AO (Call by value): mostly used by real programming languages


When does an applicative order reduction that doesn't converge into a reduction?
-- This is a halting problem.


* Abstraction extends far right
-- \x.(x z) --- which is not a redex because the body of the abstraction is x z
-- \x.x (\y.y) z w q -- which is not a redex

* Application is left-associative
-- \x. ((((x (\y.y)) z) w) q) 

-- Desugaring: remove the syntactic sugar
-- \xyz.f is equivalent to \x.\y.\z.f
----------------------------------------------------------}


1-N: the leftmost redex first
   (λx.(λy.y) x) ((λxy.x) w)     

  [(\x.(\y.y) x) ((\xy.x) w)]
->    [(\y.y) ((\xy.x) w)]
->      [((\xy.x) w)]
->          \y.w

2-N: the leftmost redex first
  (λxy.x y)(λx.x y)   
   
[(\x.\y.(x y))(\x.x y)]
[(\x.\w.(x w))(\x.x y)] -- safe substitution y -> w
->   \w.((\x.x y) w)
->   \w.[((\x.x y) w)]
->   \w.      w y


3-A: the leftmost of the innermost redex first
(λx.(λy.y) x) ((λx y.x) w)

(\x.(\y.y) x) ((\x.\y.x) w)

(\x.[(\y.y) x])((\x.\y.x) w)
->(\x.x) ((\x.\y.x) w)
->(\x.x)[((\x.\y.x) w)]
->[(\x.x)(\y.w)]
->\y.w

4-A: the leftmost of the innermost redex first
(λx y.x y) (λx.x) ((λy.y) z)
(\x.\y.x y) (\x.x) ((\y.y) z)

[(\x.\y.(x y))(\x.x)] ((\y.y) z)
->(\y. ((\x.x) y) ) ((\y.y) z)
->(\y.[((\x.x) y)]) ((\y.y) z)
->(\y.y) ((\y.y) z)
->(\y.y)[((\y.y) z)]
->[(\y.y) z]
-> z


{--------------------------------------------------EXAMPLE2
Midterm Part III.
(1) Follow the definition of the operational semantics
    especially "Congruence Rules" to figure out which redex to reduce next
(2) Use the [] to indicate the subexpression that you're reducing at each step.

<Syntax>
b ∈ Bool   ::=   true   |   false
e ∈ Exp   ::=   b   |   not e   |   cimp e e

<Reduction Rules>
not true ↦ false
not false ↦ true
cimp false true ↦ false
cimp true e ↦ true
cimp e false ↦ true

<Congruent Rules>
***** Always check where to evaluate first!! *****

    e ↦ e'
   ――――――――
not e ↦ not e'


          e ↦ e'
     ―――――――――――――――            this means that when the second arg is true, evaluate the first arg before the second one
cimp e true ↦ cimp e' true


        e₂ ↦ e₂'
      ―――――――――――――             this means that when the second arg is not true, evaluate the second arg first
 cimp e₁ e₂ ↦ cimp e₁ e₂'
----------------------------------------------------------}

Expressions to reduce

ex1:  not (cimp (not true) false)

      not (cimp [(not true)] false)
      not (cimp (false) false)
   -> not [(cimp false false)] -- cimp e false ↦ true
   -> [not true]
   -> false

ex2:  cimp (not true) (not false)

-- when there are two arguments in cimp, determine which one to evaluate first
-- since the second argument is not true, evaluate the second argument first

      cimp (not true)[(not false)]
      cimp (not true) true   -- when you have true in the second argument, reduce the first argument of cimp
      cimp[(not true)]true
    ->cimp false true
    ->false


ex3: not (cimp false (cimp false false))
     not (cimp false[(cimp false false)]) -- cimp e false ↦ true
  -> not [(cimp false true)]  -- cimp false true ↦ false
  -> not false
  -> true
