{-
Consider the following abstract syntax, which describes a language that manipulates three integer registers named A, B, and R.
-}
int	::=	(any integer)	  -- integers

reg	::=	A  |  B  |  R	  -- register names

expr	::=	int	          -- integer literal
        |	reg	          -- load from register
        |	expr + expr	  -- integer addition
        |	expr <= expr	-- less than or equal to
        |	not expr	    -- boolean negation
 			
stmt	::=	reg := expr	-- store to register
        |	if expr	conditional statement
then prog
else prog
end
|	do prog	loop until break
end
|	break	break out of a loop

prog	::=	ε  |  stmt ; prog	sequence of statements
Note that although the expression language may produce both booleans and integers, the registers may only contain integers.

Here is an example of a short program in this language that multiplies the numbers in A and B, storing the result in R. Comments, which are not reflected in the abstract syntax, are included in curly braces { … }.

A := 7;           { initialize the registers }
B := 9;
R := 0;
do
  if A <= 0 then  { loop until A is 0 }
    break;
  else
    R := R + B;   { ... add B to R }
    A := A + -1;  { ... decrement A }
  end;
end;

Note that in this assignment you are not implementing the semantics of this language.
You are only implementing and manipulating its syntax. This means that you have no way to run programs in this object language. I realize this makes testing difficult, so just do your best!


}
