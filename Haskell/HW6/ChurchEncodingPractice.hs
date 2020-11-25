

## Church encoding practice

   succ two

succ = \nfx.f(nfx)
two  = \fx.f(fx)

Therefore, succ two is "succ" applied to "two"
 = (\nfx.f (n f x)) (\fx.f (f x))
   ------------------------------ replace n in the body of abstraction with the argument, (\fx.f (f x))
-> \fx.f ((\fx.f (f x)) f x)
          --------------- replace f in the body of abstraction with the argument, f
-> \fx.f ((\x.f (f x)) x)
          -------------- replace x in the body of abstraction with the argument, x
-> \fx.f (f (f x))

The answer is equivalent to three = \fx.f(f (f x)) -- apply f to x three times
