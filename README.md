# mpolynomials

Simple multivariate polynomials in Haskell

___

```haskell
> import MultiPol

> -- a polynomial for illustration
> poly = (M (monom 2.0 [3, 1, 1]) :+: M (monom 1.0 [2, 0, 0])) :*: M (monom 5.0 [1, 1, 1])
> poly
(M (Monomial {coefficient = 2.0, powers = fromList [3, 1, 1]}) :+:
 M (Monomial {coefficient = 1.0, powers = fromList [2, 0, 0]})) :*:
M (Monomial {coefficient = 5.0, powers = fromList [1, 1, 1]})

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
