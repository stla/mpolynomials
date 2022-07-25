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

Alternatively, one can use the algebraic instances of the polynomials:

```haskell
import qualified Algebra.Additive as AA
import qualified Algebra.Module   as AM
import qualified Algebra.Ring     as AR
(2 AM.*> (x AR.^ 3) AR.* y AR.* z AA.+ x AR.* x) AR.* (4 AM.*> x AR.* y AR.* z) 
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

Or `a = Polynomial Double`:

```haskell
import MultiPol
p = lone 2 1 :: Polynomial Double
q = lone 2 2 :: Polynomial Double
x = lone 2 1 :: Polynomial (Polynomial Double)
y = lone 2 2 :: Polynomial (Polynomial Double)
poly = (p *^ x) ^+^ (q *^ y)  
poly ^*^ poly
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
```

Compact version:

```haskell
import MultiPol
import qualified Algebra.Additive as AA
import qualified Algebra.Module   as AM
import qualified Algebra.Ring     as AR
p = compact (lone 1 1) :: CompactPolynomial Double
x = compact (lone 2 1) :: CompactPolynomial (CompactPolynomial Double)
y = compact (lone 2 2) :: CompactPolynomial (CompactPolynomial Double)
poly = (p AM.*> x) AA.+ (p AM.*> y) 
poly           -- ax + ay
-- [([(1.0, [1])], [0, 1]), ([(1.0, [1])], [1, 0])]
poly AR.* poly -- a²x² + 2a²xy + a²y²
-- [([(1.0, [2])], [0, 2]), ([(2.0, [2])], [1, 1]), ([(1.0, [2])], [2, 0])]
```

Evaluation:

```haskell
import MultiPol
x = lone 3 1 :: Polynomial Double
y = lone 3 2 :: Polynomial Double
z = lone 3 3 :: Polynomial Double
poly = 2 *^ x ^*^ y ^*^ z 
-- evaluate poly at x=2, y=1, z=2
evalPoly poly [2, 1, 2]
-- 8.0
```

___

```haskell
import MultiPol
x = lone 1 :: Polynomial Double
y = lone 2 :: Polynomial Double
z = lone 3 :: Polynomial Double
poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ (4 *^ x ^*^ y ^*^ z)
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
import MultiPol
import Data.Ratio
x = lone 1 :: Polynomial Rational
y = lone 2 :: Polynomial Rational
z = lone 3 :: Polynomial Rational
((2%3) *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ ((7%4) *^ x ^*^ y ^*^ z)
-- M (Monomial {coefficient = 7 % 4, powers = fromList [3,1,1]}) 
-- :+: 
-- M (Monomial {coefficient = 7 % 6, powers = fromList [4,2,2]})
```

Or `a = Polynomial Double`:

```haskell
import MultiPol
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
prettyPol (prettyPol show "a") "x" (poly ^**^ 2)
-- "((1.0) * a^(2)) * x^(0, 2) + ((2.0) * a^(2)) * x^(1, 1) + ((1.0) * a^(2)) * x^(2, 0)"
```

Compact version:

```haskell
import MultiPol
import Prelude          hiding ((+), (*), (*>))
import Algebra.Additive as AA
import Algebra.Module   as AM
import Algebra.Ring     as AR
p = compact (lone 1) :: CompactPolynomial Double
x = compact (lone 1) :: CompactPolynomial (CompactPolynomial Double)
y = compact (lone 2) :: CompactPolynomial (CompactPolynomial Double)
poly = (p *> x) + (p *> y) 
poly           -- ax + ay
-- [([(1.0, [1])], [0, 1]), ([(1.0, [1])], [1, 0])]
poly * poly    -- a²x² + 2a²xy + a²y²
-- [([(1.0, [2])], [0, 2]), ([(2.0, [2])], [1, 1]), ([(1.0, [2])], [2, 0])]
```

Evaluation:

```haskell
import MultiPol2
x = lone 1 :: Polynomial Double
y = lone 2 :: Polynomial Double
z = lone 3 :: Polynomial Double
poly = 2 *^ x ^*^ y ^*^ z 
-- evaluate poly at x=2, y=1, z=2
evalPoly poly [2, 1, 2]
-- 8.0
```

