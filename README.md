# mpolynomials

Simple multivariate polynomials in Haskell

___

```haskell
import MultiPol
x = lone 3 1 :: Polynomial Double
y = lone 3 2 :: Polynomial Double
z = lone 3 3 :: Polynomial Double
(2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ (4 *^ x ^*^ y ^*^ z)
-- M (Monomial {coefficient = 4.0, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 8.0, powers = fromList [4,2,2]})
```

Alternatively, one can use the algebraic instance of the polynomials:

```haskell
import qualified Algebra.Additive as AA
import qualified Algebra.Ring as AR
(2 *^ x AR.* x AR.* x AR.* y AR.* z AA.+ x AR.* x) AR.* (4 *^ x AR.* y AR.* z) 
-- M (Monomial {coefficient = 4.0, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 8.0, powers = fromList [4,2,2]})
```

More generally, one can use the type `Polynomial a` as long as the type `a` has 
the instances `Eq` and `Algebra.Field` (defined in the **numeric-prelude** 
library). For example `a = Rational`:

```haskell
import MultiPol
import Data.Ratio
x = lone 3 1 :: Polynomial Rational
y = lone 3 2 :: Polynomial Rational
z = lone 3 3 :: Polynomial Rational
((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ ((7%4) *^ x ^*^ y ^*^ z)
-- M (Monomial {coefficient = 7 % 4, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 7 % 6, powers = fromList [4,2,2]})
```

___

TODO:

```haskell
> -- evaluate poly at x=2, y=1, z=2
> evalPoly poly [2, 1, 2]
720.0
```
