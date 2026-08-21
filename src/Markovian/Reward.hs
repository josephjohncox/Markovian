-- | Validated transition rewards and terminal payoffs.
module Markovian.Reward (
    Reward,
    RewardError (..),
    mkReward,
    rewardValue,
)
where

-- | A finite floating reward.
newtype Reward = Reward Double
    deriving (Eq, Show)

-- | Errors returned by 'mkReward'.
data RewardError
    = NonFiniteReward !Double
    deriving (Eq, Show)

-- | Validate a floating reward.
mkReward :: Double -> Either RewardError Reward
mkReward value
    | isNaN value || isInfinite value = Left (NonFiniteReward value)
    | otherwise = Right (Reward value)

-- | Read the validated floating reward.
rewardValue :: Reward -> Double
rewardValue (Reward value) = value
