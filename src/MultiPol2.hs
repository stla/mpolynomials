{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MultiPol2
  ( Polynomial() 
  , CompactPolynomial()
  , compact
  , Monomial(..)
  , lone
  , terms
  -- , toListOfMonomials
  -- , simplifiedListOfMonomials
  -- , fromListOfMonomials
  -- , toCanonicalForm
  , (*^)
  , (^+^)
  , (^-^)
  , (^*^)
  , (^**^)
  -- , derivPoly
  -- , monom
  , evalPoly
  , polytest 
  )
  where
import qualified Algebra.Additive as AlgAdd
import qualified Algebra.Module   as AlgMod
import qualified Algebra.Ring     as AlgRing
import           Data.Foldable    ( toList )
import           Data.Function    ( on )
import           Data.List        ( sortBy, groupBy )
import qualified Data.Sequence    as S
import           Data.Sequence    ( Seq, (><), (|>) )
import           Data.Tuple.Extra ( (&&&) )

data Monomial a = Monomial 
  { 
    coefficient :: a, 
    powers      :: Seq Int
  }
    deriving (Show, Eq)

data Polynomial a = Zero
                  | M (Monomial a)
                  | Polynomial a :+: Polynomial a
                  | Polynomial a :*: Polynomial a
                    deriving (Show)
instance (AlgRing.C a, Eq a) => Eq (Polynomial a) where
  p == q = map coefficient (toListOfMonomials $ p ^-^ q) == mempty
instance (AlgRing.C a, Eq a) => AlgAdd.C (Polynomial a) where
  p + q = addPolys p q
  zero = Zero
  negate = negatePol
instance (AlgRing.C a, Eq a) => AlgMod.C a (Polynomial a) where
  lambda *> p = scalePol lambda p
instance (AlgRing.C a, Eq a) => AlgRing.C (Polynomial a) where
  p * q = multiplyPols p q
  one = lone 0

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

addPolys :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
addPolys p q = toCanonicalForm $ p :+: q

-- | addition two polynomials
(^+^) :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(^+^) p q = p AlgAdd.+ q

negatePol :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a
negatePol pol = case pol of 
  Zero -> Zero
  M monomial -> M (negateMonomial monomial)
  pol -> fromListOfMonomials (map negateMonomial (toListOfMonomials pol))
  where
    negateMonomial :: forall a1. (AlgRing.C a1, Eq a1) => Monomial a1 -> Monomial a1
    negateMonomial monomial = Monomial { 
      coefficient = AlgAdd.negate (coefficient monomial), 
      powers = powers monomial
    }

-- | substraction
(^-^) :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(^-^) p q = p AlgAdd.- q

multiplyPols :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
multiplyPols p q = toCanonicalForm $ p :*: q

-- | multiply two polynomials
(^*^) :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(^*^) p q = p AlgRing.* q

-- | power of a polynomial
(^**^) :: (AlgRing.C a, Eq a) => Polynomial a -> Int -> Polynomial a
(^**^) p n = foldl1 (^*^) (replicate n p) 

scalePol :: (AlgRing.C a, Eq a) => a -> Polynomial a -> Polynomial a
scalePol lambda pol = if lambda == AlgAdd.zero
  then Zero 
  else case pol of
    Zero -> Zero 
    M monomial -> M (scaleMonomial monomial)
    p :+: q -> if p /= Zero && q /= Zero
      then scalePol lambda p ^+^ scalePol lambda q
      else if p == Zero
        then scalePol lambda q
        else scalePol lambda p
    p :*: q -> if p == Zero || q == Zero
      then Zero
      else scalePol lambda p ^*^ q
  where
    scaleMonomial monomial = Monomial {
                                coefficient = lambda AlgRing.* coefficient monomial
                              , powers = powers monomial
                             }

-- | scale polynomial by a scalar
(*^) :: (AlgRing.C a, Eq a) => a -> Polynomial a -> Polynomial a
(*^) lambda pol = lambda AlgMod.*> pol 

-- | variable x_i
lone :: (AlgRing.C a, Eq a) => Int -> Polynomial a
lone n = M (Monomial AlgRing.one pows)
  where
    pows = if n == 0 
      then 
        S.empty 
      else 
        S.replicate (n - 1) AlgAdd.zero |> AlgRing.one

-- | constant polynomial
constant :: (AlgRing.C a, Eq a) => a -> Polynomial a
constant x = M (Monomial x S.empty)

growSequence :: Seq Int -> Int -> Seq Int 
growSequence s n = s >< t
  where 
    m = S.length s 
    t = S.replicate (n - m) 0
 
grow :: Int -> Monomial a -> Monomial a
grow n monom = Monomial (coefficient monom) (growSequence (powers monom) n)

nvariables :: Monomial a -> Int
nvariables monom = S.length $ powers monom

-- | build a polynomial from a list of monomials
fromListOfMonomials :: (AlgRing.C a, Eq a) => [Monomial a] -> Polynomial a
fromListOfMonomials ms = if null ms
                            then Zero
                            else foldl1 (:+:) (map M ms)
  -- where
  --   n = maximum (map nvariables ms)
  --   ms' = map (grow n) ms

multMonomial :: (AlgRing.C a, Eq a) => Monomial a -> Monomial a -> Monomial a
multMonomial (Monomial ca powsa) (Monomial cb powsb) =
  Monomial (ca AlgRing.* cb) (S.zipWith (+) powsa' powsb')
  where
    n = max (S.length powsa) (S.length powsb)
    powsa' = growSequence powsa n
    powsb' = growSequence powsb n

-- polynomial to list of monomials
toListOfMonomials :: (AlgRing.C a, Eq a) => Polynomial a -> [Monomial a]
toListOfMonomials pol = case pol of
  Zero -> []
  M monomial -> if coefficient monomial == AlgAdd.zero then [] else [monomial]
  p :+: q -> harmonize $ toListOfMonomials p ++ toListOfMonomials q
  p :*: q -> harmonize $ [multMonomial monoa monob | monoa <- toListOfMonomials p,
                                                     monob <- toListOfMonomials q]
  where
    harmonize ms = map (grow (maximum (map nvariables ms))) ms

-- | list of terms of a polynomial 
terms :: (AlgRing.C a, Eq a) => Polynomial a -> [Monomial a]
terms pol = case pol of
  Zero -> []
  M monomial -> [monomial]
  p :+: q -> terms p ++ terms q
  p :*: q -> error "that should not happen"

-- polynomial to list of monomials, grouping the monomials with same powers
simplifiedListOfMonomials :: (AlgRing.C a, Eq a) => Polynomial a -> [Monomial a]
simplifiedListOfMonomials pol = map (foldl1 addMonomials) groups
  where
    groups = groupBy ((==) `on` powers)
             (sortBy (compare `on` powers) (toListOfMonomials pol))
    addMonomials :: forall a1. (AlgRing.C a1, Eq a1) => Monomial a1 -> Monomial a1 -> Monomial a1
    addMonomials monoa monob = Monomial {
                                coefficient = coefficient monoa AlgAdd.+ coefficient monob
                              , powers = powers monoa
                              }

-- canonical form of a polynomial (sum of monomials with distinct powers)
toCanonicalForm :: (AlgRing.C a, Eq a) => Polynomial a -> Polynomial a
toCanonicalForm = fromListOfMonomials . simplifiedListOfMonomials

evalMonomial :: (AlgRing.C a, Eq a) => [a] -> Monomial a -> a
evalMonomial xyz monomial =
  coefficient monomial AlgRing.* AlgRing.product (zipWith (AlgRing.^) xyz pows)
  where
    pows = toList (fromIntegral <$> powers monomial)

-- | evaluates a polynomial
evalPoly :: (AlgRing.C a, Eq a) => Polynomial a -> [a] -> a
evalPoly pol xyz = case pol of
  Zero -> AlgAdd.zero
  M mono -> evalMonomial xyz mono
  p :+: q -> evalPoly p xyz AlgAdd.+ evalPoly q xyz
  p :*: q -> error "that should not happen" --evalPoly p xyz AlgRing.* evalPoly q xyz



-- zero :: (AlgRing.C a, Eq a) => Int -> Polynomial a
-- zero n = M Monomial { coefficient = 0, powers = S.replicate n 0 }

-- derivMonomial :: Monomial -> Seq Int -> Polynomial
-- derivMonomial mono vars = let pows = powers mono in
--   case sum vars of
--     0 -> M mono
--     1 -> let (Just i) = elemIndexL 1 vars in
--          if pows !? i == Just 0 || coefficient mono == 0
--            then Zero -- zero (S.length pows)
--            else let (Just p) = pows !? i in
--                 M Monomial {coefficient = coefficient mono * fromIntegral p
--                           , powers = adjust' (subtract 1) i pows
--           }
--     _ -> let (Just i) = findIndexL (>0) vars in
--          let vars' = adjust' (subtract 1) i vars in
--          let (Just p) = pows !? i in
--          if pows !? i == Just 0 || coefficient mono == 0
--            then Zero -- zero (S.length pows)
--            else derivMonomial
--                 Monomial { coefficient = coefficient mono * fromIntegral p
--                          , powers = adjust' (subtract 1) i pows }
--                 vars'

-- derivMonomial' :: Monomial -> [Int] -> Polynomial
-- derivMonomial' mono vars = derivMonomial mono (S.fromList vars)




-- (.:+:.) :: Polynomial -> Polynomial -> Polynomial
-- (.:+:.) a b
--   | a==Zero = b
--   | b==Zero = a
--   | otherwise = a :+: b

-- (.:*:.) :: Polynomial -> Polynomial -> Polynomial
-- (.:*:.) a b = if a == Zero || b == Zero
--                  then Zero
--                  else a :*: b

-- -- | differentiation
-- derivPoly :: Polynomial -> [Int] -> Polynomial
-- derivPoly pol vars = case pol of
--   Zero    -> Zero
--   M mono  -> derivMonomial' mono vars
--   a :+: b -> derivPoly a vars .:+:. derivPoly b vars
--   a :*: b -> (derivPoly a vars .:*:. b) .:+:. (a .:*:. derivPoly b vars)





-- -- | convenient built of a monomial
-- monom :: (AlgRing.C a, Eq a) => a -> [Int] -> Monomial a
-- monom coef pows = Monomial coef (S.fromList pows)



-- evalPol :: (AlgRing.C a, Eq a) => Polynomial a -> [a] -> a
-- evalPol pol xyz = sum (map (evalMonomial xyz) monomials)
--   where
--     monomials = toListOfMonomials pol



-- evalFromListOfMonomials :: [Monomial] -> (Double, Double, Double) -> Double
-- evalFromListOfMonomials monomials xyz = sum (map (evalMonomial xyz) monomials)

polytest :: Bool
polytest = evalPoly poly [2, 3, 4] == 18816.0
  where
    x = lone 1 :: Polynomial Double
    y = lone 2 :: Polynomial Double
    z = lone 3 :: Polynomial Double
    poly = (2 *^ (x^**^3 ^*^ y ^*^ z) ^+^ (x^**^2)) ^*^ (4 *^ x ^*^ y ^*^ z)
