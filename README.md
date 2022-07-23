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

```haskell
> -- evaluate poly at x=2, y=1, z=2
> evalPoly poly [2, 1, 2]
720.0
```
