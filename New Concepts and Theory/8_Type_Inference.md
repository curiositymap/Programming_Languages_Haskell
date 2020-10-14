i# Type Inference

### How to determine type classes? 

(To be covered in the quiz 1 on 10/20)

e.g., Given e1 and e2, e1 is the function that takes an argument e2. 
Therefore, e1 should be T1 (type 1) that takes argument and gives a result. 

-- Unification Process: 
Goal: check if the type of e2 is equivalent to the type of e1's argument. 
Additionally, the type of the result should be equivalent to the outcome of e1e2.

---- This unification step is particularly tricky. However, it's heavily used in type inference. 

If there's any of these steps fails, a type error occurs. 
