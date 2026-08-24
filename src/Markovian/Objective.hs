{- | Validated floating objective parameters.

A finite-horizon discount can equal one. A contraction discount for an
infinite-horizon Bellman objective must be strictly less than one. 'Horizon'
counts transitions and uses an unbounded natural-number representation.
-}
module Markovian.Objective (
    Discount,
    ContractionDiscount,
    DiscountError (..),
    mkDiscount,
    mkContractionDiscount,
    asContractionDiscount,
    discountValue,
    contractionDiscountValue,
    Horizon,
    HorizonError (..),
    mkHorizon,
    horizonValue,
) where

import Numeric.Natural (Natural)

-- | A finite floating discount in the closed interval @[0, 1]@.
newtype Discount = Discount Double
    deriving (Eq, Ord, Show)

-- | A finite floating discount in the half-open interval @[0, 1)@.
newtype ContractionDiscount = ContractionDiscount Double
    deriving (Eq, Ord, Show)

-- | Errors returned by the floating discount constructors.
data DiscountError
    = -- | The supplied discount is NaN or infinite.
      NonFiniteDiscount !Double
    | -- | The supplied discount is outside the closed unit interval.
      DiscountOutOfRange !Double
    | -- | The supplied discount equals one and is not contractive.
      DiscountNotContractive !Double
    deriving (Eq, Show)

-- | Validate a discount for a finite-horizon objective.
mkDiscount :: Double -> Either DiscountError Discount
mkDiscount value
    | isNaN value || isInfinite value = Left (NonFiniteDiscount value)
    | value < 0 || value > 1 = Left (DiscountOutOfRange value)
    | value == 0 = Right (Discount 0)
    | otherwise = Right (Discount value)

-- | Validate a discount for a discounted infinite-horizon objective.
mkContractionDiscount :: Double -> Either DiscountError ContractionDiscount
mkContractionDiscount value = mkDiscount value >>= asContractionDiscount

-- | Refine a validated finite-horizon discount to a contraction discount.
asContractionDiscount :: Discount -> Either DiscountError ContractionDiscount
asContractionDiscount (Discount value)
    | value < 1 = Right (ContractionDiscount value)
    | otherwise = Left (DiscountNotContractive value)

-- | Read a finite-horizon discount.
discountValue :: Discount -> Double
discountValue (Discount value) = value

-- | Read a contraction discount.
contractionDiscountValue :: ContractionDiscount -> Double
contractionDiscountValue (ContractionDiscount value) = value

-- | A maximum number of model transitions.
newtype Horizon = UnsafeHorizon Natural
    deriving (Eq, Ord, Show)

-- | Errors returned by 'mkHorizon'.
data HorizonError
    = -- | The supplied transition count is negative.
      NegativeHorizon !Integer
    deriving (Eq, Show)

-- | Validate a transition horizon without imposing a machine-sized upper bound.
mkHorizon :: Integer -> Either HorizonError Horizon
mkHorizon value
    | value < 0 = Left (NegativeHorizon value)
    | otherwise = Right (UnsafeHorizon (fromInteger value))

-- | Read the maximum number of transitions.
horizonValue :: Horizon -> Natural
horizonValue (UnsafeHorizon value) = value
