newtype CompactPolynomial a = Compact (Polynomial a)
  deriving (Eq)
instance (Eq a, Show a, AlgRing.C a) => Show (CompactPolynomial a) where
  show p = show $ map (coefficient &&& (toList . powers)) (toListOfMonomials $ fromCompact p)
instance (AlgRing.C a, Eq a) => AlgAdd.C (CompactPolynomial a) where
  p + q = compact $ fromCompact p ^+^ fromCompact q
  zero = compact Zero
  negate = compact . negatePol . fromCompact
instance (AlgRing.C a, Eq a) => AlgMod.C a (CompactPolynomial a) where
  lambda *> p = compact $ lambda *^ fromCompact p
instance (AlgRing.C a, Eq a) => AlgRing.C (CompactPolynomial a) where
  p * q = compact $ fromCompact p ^*^ fromCompact q
  one = compact $ lone 0

fromCompact :: CompactPolynomial a -> Polynomial a
fromCompact (Compact p) = p 

compact :: Polynomial a -> CompactPolynomial a
compact = Compact  


polytest :: Bool
polytest = evalPoly poly [2, 3, 4] == 18816.0
  where
    x = lone 1 :: Polynomial Double
    y = lone 2 :: Polynomial Double
    z = lone 3 :: Polynomial Double
    poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ (4 *^ x ^*^ y ^*^ z)
