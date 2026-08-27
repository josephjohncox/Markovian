-- | Validated finite transition horizons.
module Markovian.Horizon (
    Horizon,
    HorizonError (..),
    mkHorizon,
    horizonValue,
) where

import Numeric.Natural (Natural)

-- | A maximum number of model transitions.
newtype Horizon = UnsafeHorizon Natural
    deriving (Eq, Ord, Show)

-- | The supplied transition count is negative.
newtype HorizonError = NegativeHorizon Integer
    deriving (Eq, Show)

-- | Validate a transition horizon without imposing a machine-sized upper bound.
mkHorizon :: Integer -> Either HorizonError Horizon
mkHorizon value
    | value < 0 = Left (NegativeHorizon value)
    | otherwise = Right (UnsafeHorizon (fromInteger value))

-- | Read the maximum number of transitions.
horizonValue :: Horizon -> Natural
horizonValue (UnsafeHorizon value) = value
