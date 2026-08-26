{- | Exact finite beliefs and post-transition POMDP filtering.

The observation kernel is evaluated from the selected action and successor
state. Prediction and conditioning remain separate public operations.
-}
module Markovian.POMDP.Exact (
    ExactBelief,
    exactBelief,
    exactBeliefFromDistribution,
    exactBeliefDistribution,
    exactBeliefOutcomes,
    ObservationTiming (..),
    ExactPOMDP,
    exactPOMDP,
    exactPOMDPModel,
    exactInitialBelief,
    exactObservationTiming,
    exactObservationDistribution,
    ExactFilteringError (..),
    predictExactBelief,
    conditionExactBelief,
    filterExactBelief,
) where

import Data.List.NonEmpty qualified as NonEmpty
import Markovian.MDP (ActionId)
import Markovian.MDP.Exact (
    ExactMDP,
    ExactModelError,
    exactSuccessorState,
    stepExactMDP,
 )
import Markovian.Probability.Exact (
    ExactDistributionError,
    ExactFiniteDist,
    ExactProb,
    exactFiniteDist,
    exactOutcomes,
    exactProbability,
 )

-- | A canonical exact finite belief with duplicate states aggregated.
newtype ExactBelief state = ExactBelief (ExactFiniteDist state)
    deriving (Eq, Show)

-- | Construct and normalize a canonical exact finite belief.
exactBelief ::
    (Eq state) =>
    [(state, Rational)] ->
    Either ExactDistributionError (ExactBelief state)
exactBelief entries = ExactBelief <$> exactFiniteDist (aggregate entries)

-- | Canonicalize an existing exact finite state distribution as a belief.
exactBeliefFromDistribution ::
    (Eq state) =>
    ExactFiniteDist state ->
    Either ExactDistributionError (ExactBelief state)
exactBeliefFromDistribution distribution =
    exactBelief
        [ (state, exactProbability mass)
        | (state, mass) <- NonEmpty.toList (exactOutcomes distribution)
        ]

-- | Read the canonical belief distribution.
exactBeliefDistribution :: ExactBelief state -> ExactFiniteDist state
exactBeliefDistribution (ExactBelief distribution) = distribution

-- | Read canonical positive belief support.
exactBeliefOutcomes :: ExactBelief state -> NonEmpty.NonEmpty (state, ExactProb)
exactBeliefOutcomes = exactOutcomes . exactBeliefDistribution

-- | Observation timing supported by the exact POMDP interface.
data ObservationTiming = ObserveAfterTransition
    deriving (Eq, Show)

-- | An exact latent-state MDP with a post-transition observation kernel.
data ExactPOMDP state action observation
    = ExactPOMDP
        !(ExactMDP state action)
        !(ExactBelief state)
        !(ActionId action -> state -> ExactFiniteDist observation)

-- | Construct an exact POMDP with post-transition observations.
exactPOMDP ::
    ExactMDP state action ->
    ExactBelief state ->
    (ActionId action -> state -> ExactFiniteDist observation) ->
    ExactPOMDP state action observation
exactPOMDP = ExactPOMDP

-- | Read the latent exact MDP.
exactPOMDPModel :: ExactPOMDP state action observation -> ExactMDP state action
exactPOMDPModel (ExactPOMDP model _ _) = model

-- | Read the exact initial belief.
exactInitialBelief :: ExactPOMDP state action observation -> ExactBelief state
exactInitialBelief (ExactPOMDP _ belief _) = belief

-- | Read the fixed observation-timing convention.
exactObservationTiming :: ExactPOMDP state action observation -> ObservationTiming
exactObservationTiming _ = ObserveAfterTransition

-- | Read one action and successor state's observation distribution.
exactObservationDistribution ::
    ExactPOMDP state action observation ->
    ActionId action ->
    state ->
    ExactFiniteDist observation
exactObservationDistribution (ExactPOMDP _ _ observe) = observe

-- | Exact prediction and conditioning failures.
data ExactFilteringError state action observation
    = ExactFilteringModelError !state !(ExactModelError action)
    | ExactFilteringDistributionError !ExactDistributionError
    | ImpossibleExactObservation !observation
    deriving (Eq, Show)

-- | Predict the successor belief before receiving an observation.
predictExactBelief ::
    (Eq state) =>
    ExactPOMDP state action observation ->
    ActionId action ->
    ExactBelief state ->
    Either (ExactFilteringError state action observation) (ExactBelief state)
predictExactBelief pomdp selected prior = do
    branches <-
        fmap
            concat
            ( traverse
                predictState
                (NonEmpty.toList (exactBeliefOutcomes prior))
            )
    mapDistributionError (exactBelief branches)
  where
    model = exactPOMDPModel pomdp

    predictState (state, beliefMass) = do
        transition <- mapModelError state (stepExactMDP model state selected)
        Right
            [ ( exactSuccessorState outcome
              , exactProbability beliefMass * exactProbability transitionMass
              )
            | (outcome, transitionMass) <- NonEmpty.toList (exactOutcomes transition)
            ]

-- | Condition a predicted belief on one post-transition observation.
conditionExactBelief ::
    (Eq state, Eq observation) =>
    ExactPOMDP state action observation ->
    ActionId action ->
    observation ->
    ExactBelief state ->
    Either (ExactFilteringError state action observation) (ExactBelief state)
conditionExactBelief pomdp selected observed predicted =
    case positive of
        [] -> Left (ImpossibleExactObservation observed)
        entries -> mapDistributionError (exactBelief entries)
  where
    positive =
        [ (state, exactProbability beliefMass * likelihood state)
        | (state, beliefMass) <- NonEmpty.toList (exactBeliefOutcomes predicted)
        , likelihood state > 0
        ]

    likelihood state =
        sum
            [ exactProbability mass
            | (candidate, mass) <-
                NonEmpty.toList
                    (exactOutcomes (exactObservationDistribution pomdp selected state))
            , candidate == observed
            ]

-- | Predict and then condition under the post-transition convention.
filterExactBelief ::
    (Eq state, Eq observation) =>
    ExactPOMDP state action observation ->
    ActionId action ->
    observation ->
    ExactBelief state ->
    Either (ExactFilteringError state action observation) (ExactBelief state)
filterExactBelief pomdp selected observed prior = do
    predicted <- predictExactBelief pomdp selected prior
    conditionExactBelief pomdp selected observed predicted

aggregate :: (Eq value) => [(value, Rational)] -> [(value, Rational)]
aggregate = foldl insert []
  where
    insert accumulated (value, mass) = go accumulated
      where
        go [] = [(value, mass)]
        go ((existing, existingMass) : remaining)
            | existing == value = (existing, existingMass + mass) : remaining
            | otherwise = (existing, existingMass) : go remaining

mapModelError ::
    state ->
    Either (ExactModelError action) value ->
    Either (ExactFilteringError state action observation) value
mapModelError state = either (Left . ExactFilteringModelError state) Right

mapDistributionError ::
    Either ExactDistributionError value ->
    Either (ExactFilteringError state action observation) value
mapDistributionError = either (Left . ExactFilteringDistributionError) Right
