{-# LANGUAGE ScopedTypeVariables #-}
module MultiPol
  ( Polynomial(..)
  , Monomial(..)
  , toListOfMonomials
  , simplifiedListOfMonomials
  , fromListOfMonomials
  , toCanonicalForm
  , (*^)
  , (^+^)
  , (^-^)
  , (^*^)
  -- , derivPoly
  , monom
  , evalPoly
  , polytest )
  where
import           Data.List
import           Data.Function
import qualified Data.Sequence as S
import           Data.Sequence (Seq, elemIndexL, (!?), adjust', findIndexL)
import           Data.Foldable (toList)

-- data Polynomial a where
--   M :: Num a => Monomial a

data Polynomial a = Zero
                  | M (Monomial a)
                  | Polynomial a :+: Polynomial a
                  | Polynomial a :*: Polynomial a
                    deriving (Show, Eq)

data Monomial a = Monomial 
  { 
    coefficient :: a, 
    powers      :: Seq Int
  }
    deriving (Show, Eq)

zero :: (Num a, Eq a) => Int -> Polynomial a
zero n = M Monomial { coefficient = 0, powers = S.replicate n 0 }

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

multMonomial :: (Num a, Eq a) => Monomial a -> Monomial a -> Monomial a
multMonomial (Monomial ca powsa) (Monomial cb powsb) =
  Monomial (ca*cb) (S.zipWith (+) powsa powsb)

-- | polynomial to list of monomials
toListOfMonomials :: (Num a, Eq a) => Polynomial a -> [Monomial a]
toListOfMonomials pol = case pol of
  Zero -> []
  M monomial -> if coefficient monomial == 0 then [] else [monomial]
  p :+: q -> toListOfMonomials p ++ toListOfMonomials q
  p :*: q -> [multMonomial monoa monob | monoa <- toListOfMonomials p,
                                         monob <- toListOfMonomials q]

-- | polynomial to list of monomials, grouping the monomials with same powers
simplifiedListOfMonomials :: (Num a, Eq a) => Polynomial a -> [Monomial a]
simplifiedListOfMonomials pol = map (foldl1 addMonomials) groups
  where
    groups = groupBy ((==) `on` powers)
             (sortBy (compare `on` powers) (toListOfMonomials pol))
    -- cantorPairing :: Int -> Int -> Int
    -- cantorPairing k1 k2 = (k1+k2)*(k1+k2+1) + 2*k2
    -- cantorPairing' :: Seq Int -> Int
    -- cantorPairing' = foldl1 cantorPairing
    addMonomials :: forall a1. (Num a1, Eq a1) => Monomial a1 -> Monomial a1 -> Monomial a1
    addMonomials monoa monob = Monomial {
                                coefficient = coefficient monoa + coefficient monob
                              , powers = powers monoa
                              }

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

-- | build a polynomial from a list of monomials
fromListOfMonomials :: (Num a, Eq a) => [Monomial a] -> Polynomial a
fromListOfMonomials ms = if null ms
                            then Zero
                            else foldl1 (:+:) (map M ms)

-- | canonical form of a polynomial (sum of monomials)
toCanonicalForm :: (Num a, Eq a) => Polynomial a -> Polynomial a
toCanonicalForm = fromListOfMonomials . simplifiedListOfMonomials

negatePol :: (Num a, Eq a) => Polynomial a -> Polynomial a
negatePol pol = case pol of 
  Zero -> Zero
  M monomial -> M (negateMonomial monomial)
  pol -> fromListOfMonomials (map negateMonomial (toListOfMonomials pol))
  where
    negateMonomial :: forall a1. (Num a1, Eq a1) => Monomial a1 -> Monomial a1
    negateMonomial monomial = Monomial { 
      coefficient = negate (coefficient monomial), 
      powers = powers monomial
    }

(^-^) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(^-^) p q = p :+: negatePol q

(^+^) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(^+^) p q = p :+: q

(^*^) :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
(^*^) p q = p :*: q

-- | scale polynomial by a scalar
(*^) :: (Num a, Eq a) => a -> Polynomial a -> Polynomial a
(*^) lambda pol = if lambda == 0
  then Zero 
  else case pol of
    Zero -> Zero 
    M monomial -> M (scaleMonomial monomial)
    p :+: q -> if p /= Zero && q /= Zero
      then (*^) lambda p :+: (*^) lambda q
      else if p == Zero
        then (*^) lambda q
        else (*^) lambda p
    p :*: q -> if p == Zero || q == Zero
      then Zero
      else (*^) lambda p :*: q
  where
    scaleMonomial monomial = Monomial {
                                    coefficient = lambda * coefficient monomial
                                  , powers = powers monomial
                             }

-- | convenient built of a monomial
monom :: (Num a, Eq a) => a -> [Int] -> Monomial a
monom coef pows = Monomial coef (S.fromList pows)

-- xpol :: Polynomial
-- xpol = M Monomial {coefficient = 1, powers = (1,0,0)}
--
-- ypol :: Polynomial
-- ypol = M Monomial {coefficient = 1, powers = (0,1,0)}

evalMonomial :: (Num a, Eq a) => [a] -> Monomial a -> a
evalMonomial xyz monomial =
  coefficient monomial * product (zipWith (^) xyz pows)
  where
    pows = toList (powers monomial)

evalPol :: (Num a, Eq a) => Polynomial a -> [a] -> a
evalPol pol xyz = sum (map (evalMonomial xyz) monomials)
  where
    monomials = toListOfMonomials pol

-- | evaluates a polynomial
evalPoly :: (Num a, Eq a) => Polynomial a -> [a] -> a
evalPoly pol xyz = case pol of
  Zero -> 0
  M mono -> evalMonomial xyz mono
  p :+: q -> evalPoly p xyz + evalPoly q xyz
  p :*: q -> evalPoly p xyz * evalPoly q xyz

-- evalFromListOfMonomials :: [Monomial] -> (Double, Double, Double) -> Double
-- evalFromListOfMonomials monomials xyz = sum (map (evalMonomial xyz) monomials)

polytest :: Polynomial Double
polytest = (M (monom 2 [3,1,1]) :+: M (monom 1 [2,0,0])) :*: M (monom 5 [1,1,1])
