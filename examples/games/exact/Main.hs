module Main (main) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.Category.Finite.Object
import Markovian.Game.Correlated.Exact
import Markovian.Game.NormalForm.Exact
import Markovian.Game.Profile.Finite

-- This example verifies one represented candidate. It does not find or prove
-- the existence of an equilibrium.
data Player = Row | Column deriving (Eq, Show)
data Action = Heads | Tails deriving (Eq, Show)

limits :: GameLimits
limits = gameLimits 2 2 4 16 200 256 1

main :: IO ()
main = do
    owners <- checked (finiteObject [Row, Column])
    actions <- checked (finiteObject [Heads, Tails])
    product_ <- checked (ownedProduct limits owners [(Row, actions), (Column, actions)])
    payoffs <- traverse (payoff owners) (NonEmpty.toList (finiteObjectValues (ownedProfiles product_)))
    game <- checked (exactNormalGame limits product_ payoffs)
    half <- checked (exactSimplex limits actions [(Heads, 1 / 2), (Tails, 1 / 2)])
    mixed <- checked (exactMixedProfile limits product_ [(Row, half), (Column, half)])
    nash <- checked (checkMixedNash limits game mixed)
    masses <- traverse (profileMass mixed) (NonEmpty.toList (finiteObjectValues (ownedProfiles product_)))
    device <- checked (exactCorrelationDevice limits product_ masses)
    ce <- checked (checkCorrelatedEquilibrium limits game device)
    putStrLn ("mixed Nash candidate: " ++ show (mixedNashSatisfied nash))
    putStrLn ("induced product CE candidate: " ++ show (correlatedEquilibriumSatisfied ce))
  where
    payoff owners profile = do
        let same = profileChoice profile Row == profileChoice profile Column
        values <- checked (exactPlayerValues limits owners [(Row, if same then 1 else -1), (Column, if same then -1 else 1)])
        pure (profile, values)
    profileMass mixed profile = do
        mass <- checked (mixedProfileProbability limits mixed profile)
        case mass of
            Nothing -> fail "profile escaped its owned product"
            Just value -> pure (profile, value)

checked :: (Show error) => Either error value -> IO value
checked = either (fail . show) pure
