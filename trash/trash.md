Compact version:

```haskell
import Math.Algebra.MultiPol
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
