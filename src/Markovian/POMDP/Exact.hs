{-# LANGUAGE RoleAnnotations #-}

{- | Exact finite beliefs and post-transition POMDP filtering.

The observation kernel is evaluated from the selected action and successor
state. Prediction and conditioning remain separate public operations and
delegate their generic normalization to "Markovian.Bayesian.Exact".
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
import Markovian.Action (ActionId)
import Markovian.Bayesian.Exact (
    ExactConditioningError (..),
    ExactDistributionBayesianError (..),
    canonicalExactDistribution,
    conditionExactDistribution,
    pushforwardExactDistribution,
 )
import Markovian.MDP.Exact (
    ExactMDP,
    ExactModelError,
    exactSuccessorState,
    stepExactMDP,
 )
import Markovian.Probability.Exact (
    ExactBindError,
    ExactDistributionError,
    ExactFiniteDist,
    ExactProb,
    exactOutcomes,
    exactProbability,
 )

-- | A canonical exact finite belief with duplicate states aggregated.
type role ExactBelief nominal

newtype ExactBelief state = ExactBelief (ExactFiniteDist state)
    deriving (Eq, Show)

-- | Construct and normalize a canonical exact finite belief.
exactBelief ::
    (Eq state) =>
    [(state, Rational)] ->
    Either ExactDistributionError (ExactBelief state)
exactBelief entries = ExactBelief <$> canonicalExactDistribution entries

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
    | ExactFilteringPredictionBindError !(ExactBindError (state, ExactModelError action))
    | ExactFilteringConditioningBindError !(ExactBindError ExactDistributionError)
    | ImpossibleExactObservation !observation
    deriving (Eq, Show)

-- | Predict the successor belief before receiving an observation.
predictExactBelief ::
    (Eq state) =>
    ExactPOMDP state action observation ->
    ActionId action ->
    ExactBelief state ->
    Either (ExactFilteringError state action observation) (ExactBelief state)
predictExactBelief pomdp selected prior =
    case pushforwardExactDistribution
        (exactBeliefDistribution prior)
        predictState of
        Left (ExactDistributionKernelError (state, modelError)) ->
            Left (ExactFilteringModelError state modelError)
        Left (ExactDistributionBindError bindError) ->
            Left (ExactFilteringPredictionBindError bindError)
        Left (ExactDistributionNormalizationError distributionError) ->
            Left (ExactFilteringDistributionError distributionError)
        Right predicted -> Right (ExactBelief predicted)
  where
    model = exactPOMDPModel pomdp
    predictState state =
        case stepExactMDP model state selected of
            Left modelError -> Left (state, modelError)
            Right transition ->
                Right
                    ( fmap
                        exactSuccessorState
                        transition
                    )

-- | Condition a predicted belief on one post-transition observation.
conditionExactBelief ::
    (Eq state, Eq observation) =>
    ExactPOMDP state action observation ->
    ActionId action ->
    observation ->
    ExactBelief state ->
    Either (ExactFilteringError state action observation) (ExactBelief state)
conditionExactBelief pomdp selected observed predicted =
    case conditionExactDistribution
        observed
        (exactBeliefDistribution predicted)
        (exactObservationDistribution pomdp selected) of
        Left (ExactZeroEvidence impossible) -> Left (ImpossibleExactObservation impossible)
        Left (ExactConditioningBindError bindError) ->
            Left (ExactFilteringConditioningBindError bindError)
        Left (ExactConditioningNormalizationError distributionError) ->
            Left (ExactFilteringDistributionError distributionError)
        Right posterior -> Right (ExactBelief posterior)

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
