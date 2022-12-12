# mpolynomials

Simple multivariate polynomials in Haskell.

*This package won't be developed anymore*. Please use [**hspray**](https://github.com/stla/hspray).

___


```haskell
import Math.Algebra.MultiPol
x = lone 1 :: Polynomial Double
y = lone 2 :: Polynomial Double
z = lone 3 :: Polynomial Double
poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ (4 *^ (x ^*^ y ^*^ z))
poly
-- M (Monomial {coefficient = 4.0, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 8.0, powers = fromList [4,2,2]})
prettyPol show "x" poly
-- "(4.0) * x^(3, 1, 1) + (8.0) * x^(4, 2, 2)"
```

More generally, one can use the type `Polynomial a` as long as the type `a` has 
the instances `Eq` and `Algebra.Ring` (defined in the **numeric-prelude** 
library). For example `a = Rational`:

```haskell
import Math.Algebra.MultiPol
import Data.Ratio
x = lone 1 :: Polynomial Rational
y = lone 2 :: Polynomial Rational
z = lone 3 :: Polynomial Rational
((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^+^ x^**^2) ^*^ ((7%4) *^ (x ^*^ y ^*^ z))
-- M (Monomial {coefficient = 7 % 4, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 7 % 6, powers = fromList [4,2,2]})
```

Or `a = Polynomial Double`:

```haskell
import Math.Algebra.MultiPol
p = lone 1 :: Polynomial Double
x = lone 1 :: Polynomial (Polynomial Double)
y = lone 2 :: Polynomial (Polynomial Double)
poly = (p *^ x) ^+^ (p *^ y)  
poly ^**^ 2 
-- (M (Monomial {
--   coefficient = M (Monomial {coefficient = 1.0, powers = fromList [0,2]}), 
--   powers = fromList [0,2]}) 
-- :+: 
--  M (Monomial {
--    coefficient = M (Monomial {coefficient = 2.0, powers = fromList [1,1]}), 
--    powers = fromList [1,1]})) 
-- :+: 
--  M (Monomial {
--    coefficient = M (Monomial {coefficient = 1.0, powers = fromList [2,0]}), 
--    powers = fromList [2,0]})
prettyPol (prettyPol show "a") "X" (poly ^**^ 2)
-- "((1.0) * a^(2)) * X^(0, 2) + ((2.0) * a^(2)) * X^(1, 1) + ((1.0) * a^(2)) * X^(2, 0)"
```

Evaluation:

```haskell
import Math.Algebra.MultiPol
x = lone 1 :: Polynomial Double
y = lone 2 :: Polynomial Double
z = lone 3 :: Polynomial Double
poly = 2 *^ (x ^*^ y ^*^ z) 
-- evaluate poly at x=2, y=1, z=2
evalPoly poly [2, 1, 2]
-- 8.0
```

