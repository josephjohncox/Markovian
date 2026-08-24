{- | Exact rewards for reference interpreters and literal law tests.

Every 'Rational' is finite, so construction is total. This type remains distinct
from the floating 'Markovian.Reward.Reward' type.
-}
module Markovian.Reward.Exact (
    ExactReward,
    exactReward,
    exactRewardValue,
) where

-- | An exact rational transition reward or terminal payoff.
newtype ExactReward = ExactReward Rational
    deriving (Eq, Ord, Show)

-- | Construct an exact reward.
exactReward :: Rational -> ExactReward
exactReward = ExactReward

-- | Read the exact rational reward.
exactRewardValue :: ExactReward -> Rational
exactRewardValue (ExactReward value) = value
