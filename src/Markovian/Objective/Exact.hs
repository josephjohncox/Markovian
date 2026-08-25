{- | Exact objective parameters for reference interpreters and law tests.

These rational discounts remain distinct from the floating objective values in
"Markovian.Objective".
-}
module Markovian.Objective.Exact (
    ExactDiscount,
    ExactContractionDiscount,
    ExactDiscountError (..),
    mkExactDiscount,
    mkExactContractionDiscount,
    asExactContractionDiscount,
    exactDiscountValue,
    exactContractionDiscountValue,
    ExactFiniteObjective,
    exactFiniteObjective,
    exactObjectiveHorizon,
    exactObjectiveDiscount,
) where

import Markovian.Objective (Horizon)

-- | An exact rational discount in the closed interval @[0, 1]@.
newtype ExactDiscount = ExactDiscount Rational
    deriving (Eq, Ord, Show)

-- | An exact rational discount in the half-open interval @[0, 1)@.
newtype ExactContractionDiscount = ExactContractionDiscount Rational
    deriving (Eq, Ord, Show)

-- | Errors returned by the exact discount constructors.
data ExactDiscountError
    = -- | The supplied rational is outside the closed unit interval.
      ExactDiscountOutOfRange !Rational
    | -- | The supplied rational equals one and is not contractive.
      ExactDiscountNotContractive !Rational
    deriving (Eq, Show)

-- | Validate an exact discount for a finite-horizon objective.
mkExactDiscount :: Rational -> Either ExactDiscountError ExactDiscount
mkExactDiscount value
    | value < 0 || value > 1 = Left (ExactDiscountOutOfRange value)
    | otherwise = Right (ExactDiscount value)

-- | Validate an exact discount for an infinite-horizon objective.
mkExactContractionDiscount :: Rational -> Either ExactDiscountError ExactContractionDiscount
mkExactContractionDiscount value = mkExactDiscount value >>= asExactContractionDiscount

-- | Refine an exact finite-horizon discount to a contraction discount.
asExactContractionDiscount :: ExactDiscount -> Either ExactDiscountError ExactContractionDiscount
asExactContractionDiscount (ExactDiscount value)
    | value < 1 = Right (ExactContractionDiscount value)
    | otherwise = Left (ExactDiscountNotContractive value)

-- | Read an exact finite-horizon discount.
exactDiscountValue :: ExactDiscount -> Rational
exactDiscountValue (ExactDiscount value) = value

-- | Read an exact contraction discount.
exactContractionDiscountValue :: ExactContractionDiscount -> Rational
exactContractionDiscountValue (ExactContractionDiscount value) = value

-- | A finite transition horizon paired with an exact discount.
data ExactFiniteObjective = UnsafeExactFiniteObjective !Horizon !ExactDiscount
    deriving (Eq, Show)

-- | Construct a named exact finite-horizon objective from validated values.
exactFiniteObjective :: Horizon -> ExactDiscount -> ExactFiniteObjective
exactFiniteObjective = UnsafeExactFiniteObjective

-- | Read an exact objective's maximum transition count.
exactObjectiveHorizon :: ExactFiniteObjective -> Horizon
exactObjectiveHorizon (UnsafeExactFiniteObjective horizon _) = horizon

-- | Read an exact objective's transition discount.
exactObjectiveDiscount :: ExactFiniteObjective -> ExactDiscount
exactObjectiveDiscount (UnsafeExactFiniteObjective _ discount) = discount
