# mpolynomials

Simple multivariate polynomials in Haskell

___

```haskell
import MultiPol

-- a polynomial for illustration
x = lone 3 1 :: Polynomial Double
y = lone 3 2 :: Polynomial Double
z = lone 3 3 :: Polynomial Double
(2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ (4 *^ x ^*^ y ^*^ z)
-- M (Monomial {coefficient = 5.0, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 8.0, powers = fromList [4,2,2]})
___________________________________________________________________

(2 *^ x AR.* x AR.* x AR.* y AR.* z AA.+ x AR.* x) AR.* (4 *^ x AR.* y AR.* z) 

> -- multiply by a scalar
> 2 *^ poly
(M (Monomial {coefficient = 4.0, powers = fromList [3, 1, 1]}) :+:
 M (Monomial {coefficient = 2.0, powers = fromList [2, 0, 0]})) :*:
 M (Monomial {coefficient = 5.0, powers = fromList [1, 1, 1]})

> -- the monomials composing the polynomial (terms)
> toListOfMonomials poly
[ Monomial {coefficient = 10.0, powers = fromList [4, 2, 2]}
, Monomial {coefficient = 5.0, powers = fromList [3, 1, 1]} ]

> -- groups the monomials with same powers - no difference here
> simplifiedListOfMonomials poly
[ Monomial {coefficient = 5.0, powers = fromList [3, 1, 1]}, 
, Monomial {coefficient = 10.0, powers = fromList [4, 2, 2]} ]

> -- canonical form (sum of monomials)
> toCanonicalForm poly
M (Monomial {coefficient = 5.0, powers = fromList [3, 1, 1]}) :+:
M (Monomial {coefficient = 10.0, powers = fromList [4, 2, 2]})

> -- evaluate poly at x=2, y=1, z=2
> evalPoly poly [2, 1, 2]
720.0

> -- derivate two times in x and one time in y
> derivPoly poly [2, 1, 0]
M (Monomial {coefficient = 12.0, powers = fromList [1, 0, 1]}) :*:
M (Monomial {coefficient = 5.0, powers = fromList [1, 1, 1]})

> toCanonicalForm $ derivPoly poly [2, 1, 0]
M (Monomial {coefficient = 60.0, powers = fromList [2, 1, 2]})
```
